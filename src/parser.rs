#![allow(unused)]

use crate::ast::*;
use crate::lexer;
use crate::lexer::Token as T;
use crate::lexer::Token;

macro_rules! b {
    ($e:expr) => {
        Box::new($e)
    };
}

macro_rules! t {
    ($name:ident, $token:ident, $thing:ident) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<$thing<'t>> {
            match p.next() {
                (Some(T::$token(x)), s) => Some($thing(S(x, s))),
                _ => p.raise_(stringify!($thing)),
            }
        }
    };
}

t!(name, Lower, Name);
t!(proper, Upper, ProperName);
t!(qual, Qual, Qual);
t!(number, Number, Number);
t!(char, Char, Char);
t!(op, Symbol, Op);
t!(hole, Hole, Hole);

fn boolean<'t>(p: &mut P<'t>) -> Option<Boolean<'t>> {
    match p.next() {
        (Some(T::Lower(x @ ("true" | "false"))), s) => Some(Boolean(S(x, s))),
        _ => p.raise_("Boolean"),
    }
}

fn string<'t>(p: &mut P<'t>) -> Option<Str<'t>> {
    match p.next() {
        (Some(T::String(x) | T::RawString(x)), s) => Some(Str(S(x, s))),
        _ => p.raise_("String | RawString"),
    }
}

fn label<'t>(p: &mut P<'t>) -> Option<Label<'t>> {
    Some(match p.peek() {
        (Some(T::Lower(n) | T::String(n) | T::RawString(n)), s) => Label(S(n, s)),
        _ => return None,
    })
}

macro_rules! kw {
    ($name:ident, $kw:pat) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<()> {
            p.expect(|x| matches!(x, $kw), stringify!($kw))
        }
    };
}

kw!(kw_lp, Token::LeftParen);
kw!(kw_rp, Token::RightParen);
kw!(kw_lb, Token::LeftBrace);
kw!(kw_rb, Token::RightBrace);
kw!(kw_ls, Token::LeftSquare);
kw!(kw_rs, Token::RightSquare);
kw!(kw_left_arrow, Token::LeftArrow);
kw!(kw_right_arrow, Token::RightArrow);
kw!(kw_right_imply, Token::RightFatArrow);
kw!(kw_coloncolon, Token::Symbol("::"));
kw!(kw_colon, Token::Symbol(":"));
kw!(kw_tick, Token::Tick);
kw!(kw_comma, Token::Comma);

kw!(kw_left_imply, T::Symbol("<="));
kw!(kw_at, T::Symbol("@"));
kw!(kw_pipe, T::Symbol("|"));
kw!(kw_dot, T::Symbol("."));
kw!(kw_eq, T::Symbol("="));
kw!(kw_dotdot, T::Symbol(".."));
kw!(kw_underscore, T::Symbol("_"));
kw!(kw_minus, T::Symbol("-"));
kw!(kw_backslash, T::Symbol("\\"));

kw!(kw_module, T::Lower("module"));
kw!(kw_where, T::Where);
kw!(kw_class, T::Lower("class"));
kw!(kw_as, T::Lower("as"));
kw!(kw_import, T::Lower("import"));
kw!(kw_hiding, T::Lower("hiding"));
kw!(kw_type, T::Lower("type"));
kw!(kw_newtype, T::Lower("newtype"));
kw!(kw_data, T::Lower("data"));
kw!(kw_true, T::Lower("true"));
kw!(kw_false, T::Lower("false"));
kw!(kw_forall, T::Lower("forall"));
kw!(kw_let, T::Let);
kw!(kw_in, T::In);
kw!(kw_if, T::If);
kw!(kw_then, T::Then);
kw!(kw_else, T::Else);
kw!(kw_case, T::Case);
kw!(kw_of, T::Of);
kw!(kw_do, T::Do);
kw!(kw_ado, T::Ado);

kw!(kw_begin, T::LayBegin);
kw!(kw_end, T::LayEnd);
kw!(kw_sep, T::LaySep);
kw!(kw_top, T::LayTop);

fn symbol<'t>(p: &mut P<'t>) -> Option<Symbol<'t>> {
    kw_lp(p)?;
    let sym = op(p)?.0;
    kw_rp(p)?;
    Some(Symbol::<'t>(sym))
}

macro_rules! q {
    ($name:ident, $inner:expr, $token:expr, $thing:ident) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<$thing<'t>> {
            let start = p.span();
            let xs = quals(p);
            let end = p.span();
            let x = $inner(p)?;
            Some($token(S(xs, start.merge(end)), x))
        }
    };
}

fn quals<'t>(p: &mut P<'t>) -> Vec<Qual<'t>> {
    let mut out = Vec::new();
    loop {
        match p.peek().0 {
            Some(T::Qual(_)) => out.push(qual(p).unwrap()),
            _ => break,
        }
    }
    out
}

q!(qname, name, QName, QName);
q!(qproper, proper, QProperName, QProperName);
q!(qsymbol, symbol, QSymbol, QSymbol);
q!(qop, op, QOp, QOp);

fn header<'t>(p: &mut P<'t>) -> Option<Header<'t>> {
    while matches!(p.peek().0, Some(T::LayTop)) {
        p.skip();
    }
    kw_module(p)?;
    let name = qproper(p)?;
    let exports = exports(p);
    // TODO: recover on `where`
    p.recover(|x| matches!(x, T::Lower("where") | T::LayTop))
        .ok()?;
    kw_where(p)?;
    let imports = imports(p);
    Some(Header(name, exports, imports))
}

// TODO pick the errros from the branch that moved the most consumed tokens
macro_rules! choice {
    ($p:ident : $e:expr, $($t:expr),*) => {
        {
            let (p__, x) = match () {
                // TODO: If this was a proc-macro the code would be twice as fast
                // TODO: Rust doesn't have if-let guards on matches
            $(
                _ if matches!({
                    let mut p_ = $p.fork();
                    let out = $t(&mut p_);
                    (p_, out)
                }, (_, Some(_))) =>  {
                    let mut p_ = $p.fork();
                    let out = $t(&mut p_);
                    (p_, Ok(out))
                }
            ),*
                _ => {
                    let mut p_ = $p.fork();
                    p_.raise_::<usize>($e);
                    (p_, Err(None))
                }
            };
            match x {
                Ok(e) => {
                    *$p = p__;
                    e
                }
                Err(e) => {
                    e
                }
            }
        }
    };
    ($p:ident : $e:expr, $($t:expr),* ,) => { choice!($p: $e, $($t),*) };

    // ($p:ident, $($t:expr),*) => { choice!($p : stringify!($($t),*), $($t),*) };
}

fn many<'t, FE, E>(p: &mut P<'t>, err: &'static str, e: FE) -> Vec<E>
where
    FE: Fn(&mut P<'t>) -> Option<E>,
{
    let mut out = Vec::new();
    while let Some(ee) = choice!(p : err, e, |_| None::<E>) {
        out.push(ee);
    }
    out
}

fn sep<'t, FS, FE, E, S>(p: &mut P<'t>, err: &'static str, s: FS, e: FE) -> Vec<E>
where
    FS: Fn(&mut P<'t>) -> Option<S>,
    FE: Fn(&mut P<'t>) -> Option<E>,
{
    let mut out = Vec::new();
    loop {
        if let Some(ee) = e(p) {
            out.push(ee);
        } else {
            break;
        }
        if choice!(p: err, s, |_| None::<S>).is_some() {
            continue;
        } else {
            break;
        }
    }
    out
}

fn exports<'t>(p: &mut P<'t>) -> Vec<Export<'t>> {
    fn f<'t>(p: &mut P<'t>) -> Option<Vec<Export<'t>>> {
        kw_lp(p)?;
        let exports = sep(p, "export", kw_comma, export);
        kw_rp(p)?;
        Some(exports)
    }
    choice!(p: "'export", f, |_| { Some(Vec::<Export<'t>>::new()) }).unwrap()
}

fn export<'t>(p: &mut P<'t>) -> Option<Export<'t>> {
    choice!(
        p: "export",
        |p: &mut _| {
            kw_type(p)?;
            Some(Export::TypSymbol(symbol(p)?))
        },
        |p: &mut _| {
            kw_class(p)?;
            Some(Export::Class(proper(p)?))
        },
        |p: &mut _| {
            kw_module(p)?;
            Some(Export::Module(qproper(p)?))
        },
        |p: &mut _| { Some(Export::Value(name(p)?)) },
        |p: &mut _| { Some(Export::Symbol(symbol(p)?)) },
        |p: &mut _| {
            let n = proper(p)?;
            let ms = data_members(p)?;
            Some(Export::TypDat(n, ms))
        },
        |p: &mut _| { Some(Export::Typ(proper(p)?)) }
    )
}

fn data_members<'t>(p: &mut P<'t>) -> Option<DataMember<'t>> {
    kw_lp(p)?;
    let out = match p.peek().0 {
        Some(T::Symbol("..")) => {
            p.skip();
            DataMember::All
        }
        Some(T::LeftParen) => DataMember::Some(Vec::new()),
        Some(_) => DataMember::Some(sep(p, "data_member.proper", kw_comma, proper)),
        _ => p.raise_("EoF")?,
    };
    kw_rp(p)?;
    Some(out)
}

fn skip_to_top<'t>(p: &mut P<'t>) -> Option<()> {
    p.recover(|t| matches!(t, T::LayTop)).ok()
}

fn imports<'t>(p: &mut P<'t>) -> Vec<ImportDecl<'t>> {
    let mut out = Vec::new();
    while matches!(p.peek2t(), (Some(T::LayTop), Some(T::Lower("import")))) {
        p.skip();
        if let Some(import) = import_decl(p) {
            out.push(import);
        }
        if skip_to_top(p).is_none() {
            break;
        }
    }
    out
}

fn import_decl<'t>(p: &mut P<'t>) -> Option<ImportDecl<'t>> {
    kw_import(p)?;
    let name = qproper(p)?;
    Some(match p.peekt() {
        Some(T::Lower("as")) => {
            kw_as(p)?;
            let as_name = qproper(p)?;
            ImportDecl::As(name, as_name)
        }
        Some(T::Lower("hiding")) => {
            kw_hiding(p)?;
            kw_lp(p)?;
            let imports = sep(p, "hiding imports", kw_comma, import);
            kw_rp(p)?;
            ImportDecl::Hiding(name, imports)
        }
        Some(T::LeftParen) => {
            kw_lp(p)?;
            let imports = sep(p, "imports", kw_comma, import);
            kw_rp(p)?;
            ImportDecl::Multiple(name, imports)
        }
        _ => ImportDecl::Bulk(name),
    })
}

fn import<'t>(p: &mut P<'t>) -> Option<Import<'t>> {
    choice!(
        p: "'import",
        |p: &mut _| {
            kw_type(p)?;
            Some(Import::TypSymbol(symbol(p)?))
        },
        |p: &mut _| {
            kw_class(p)?;
            Some(Import::Class(proper(p)?))
        },
        |p: &mut _| { Some(Import::Value(name(p)?)) },
        |p: &mut _| { Some(Import::Symbol(symbol(p)?)) },
        |p: &mut _| {
            let n = proper(p)?;
            let ms = data_members(p)?;
            Some(Import::TypDat(n, ms))
        },
        |p: &mut _| { Some(Import::Typ(proper(p)?)) }
    )
}

fn typ_atom<'t>(p: &mut P<'t>) -> Option<Typ<'t>> {
    choice!(
        p: "typ_atom",
        |p: &mut P<'t>| {
            let span = p.span();
            kw_underscore(p)?;
            Some(Typ::Wildcard(span))
        },
        |p| { Some(Typ::Var(name(p)?)) },
        |p| { Some(Typ::Constructor(qproper(p)?)) },
        |p| { Some(Typ::Symbol(qsymbol(p)?)) },
        |p| { Some(Typ::Str(string(p)?)) },
        // |p| { Some(Typ::Int(int(p)?)) },
        |p| { Some(Typ::Hole(hole(p)?)) },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_lb(p)?;
            let r = row(p)?;
            kw_rb(p)?;
            let end = p.span();
            Some(Typ::Record(S(r, start.merge(end))))
        },
        // NOTE: ( a :: B ) is considered a row-type in this conflict
        |p: &mut P<'t>| {
            let start = p.span();
            kw_lp(p)?;
            let r = row(p)?;
            kw_rp(p)?;
            let end = p.span();
            Some(Typ::Row(S(r, start.merge(end))))
        },
        |p: &mut _| {
            kw_lp(p)?;
            let r = typ(p)?;
            kw_rp(p)?;
            Some(r)
        }
    )
}

// Higher binds tighter
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Prec {
    L(usize),
    R(usize),
}

impl Prec {
    fn zero() -> usize {
        0
    }

    fn next(&self, current: usize) -> Option<usize> {
        match (current, self) {
            (s, Self::L(o)) if o > &s => Some(s + 1),
            (s, Self::R(o)) if o > &s => Some(s + 1),
            (s, Self::R(o)) if o == &s => Some(s),
            _ => None,
        }
    }

    fn prec(&self) -> usize {
        match self {
            Self::L(a) | Self::R(a) => *a,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum TypOp<'t> {
    Kind,
    Arr,
    FatArr,
    Op(QOp<'t>),
    App,
}

fn typ_op<'t>(p: &mut P<'t>) -> Option<TypOp<'t>> {
    choice!(
        p: "typ_op",
        |p| {
            kw_coloncolon(p)?;
            Some(TypOp::Kind)
        },
        |p| {
            kw_right_arrow(p)?;
            Some(TypOp::Arr)
        },
        |p| {
            kw_right_imply(p)?;
            Some(TypOp::FatArr)
        },
        |p| { Some(TypOp::Op(qop(p)?)) },
        |p: &mut P<'t>| {
            typ_atom(&mut p.fork())?;
            Some(TypOp::App)
        },
        |_| None::<TypOp<'t>>
    )
}

fn typ_fop<'t>(t: &TypOp<'t>) -> Prec {
    use Prec::*;
    match t {
        TypOp::Kind => L(0),
        TypOp::FatArr => R(1),
        TypOp::Arr => R(1),
        // This is not right - we can maybe do better
        TypOp::Op(_) => L(3),
        TypOp::App => L(4),
    }
}

fn typ_mrg<'t>(p: &mut P<'t>, op: TypOp<'t>, lhs: Typ<'t>, rhs: Typ<'t>) -> Typ<'t> {
    match op {
        TypOp::Kind => Typ::Kinded(b!(lhs), b!(rhs)),
        TypOp::Arr => Typ::Arr(b!(lhs), b!(rhs)),
        TypOp::FatArr => {
            if let Some(lhs) = lhs.clone().to_constraint() {
                Typ::Constrained(lhs, b!(rhs))
            } else {
                p.raise(Serror::NotAConstraint(lhs.span()));
                Typ::Arr(b!(lhs), b!(rhs))
            }
        }
        TypOp::Op(op) => Typ::Op(b!(lhs), op, b!(rhs)),
        TypOp::App => Typ::App(b!(lhs), b!(rhs)),
    }
}

fn top_typ<'t>(p: &mut P<'t>) -> Option<Typ<'t>> {
    choice!(
        p: "top_typ",
        |p: &mut _| {
            kw_forall(p)?;
            let vars = typ_var_bindings(p);
            kw_dot(p)?;
            let inner = typ(p)?;
            Some(Typ::Forall(vars, b!(inner)))
        },
        typ_atom
    )
}

fn typ<'t>(p: &mut P<'t>) -> Option<Typ<'t>> {
    let lhs = top_typ(p)?;
    pratt_typ(p, lhs, Prec::zero())
}

fn pratt_typ<'t>(p: &mut P<'t>, mut lhs: Typ<'t>, prec: usize) -> Option<Typ<'t>> {
    while let Some(outer_lookahead) = (|p: &mut P<'t>| {
        let op = typ_op(&mut p.fork())?;
        if typ_fop(&op).prec() >= prec {
            if !matches!(op, TypOp::App) {
                let _ = typ_op(p)?;
            }
            Some(op)
        } else {
            None
        }
    })(p)
    {
        // Make the check a peek
        let mut rhs = top_typ(p)?;
        while let Some(next) = (|p: &mut P<'t>| {
            let op = typ_op(&mut p.fork())?;
            typ_fop(&op).next(typ_fop(&outer_lookahead).prec())
        })(p)
        {
            let i = p.i;
            rhs = pratt_typ(p, rhs, next)?;
            // assert_ne!(i, p.i, "STUCK TYP!");
        }
        lhs = typ_mrg(p, outer_lookahead, lhs, rhs);
    }
    Some(lhs)
}

fn typ_var_binding<'t>(p: &mut P<'t>) -> Option<TypVarBinding<'t>> {
    let is_paren = matches!(p.peekt(), Some(T::LeftParen));
    if is_paren {
        kw_lp(p)?;
    }

    let is_at = matches!(p.peekt(), Some(T::Symbol("@")));
    if is_at {
        kw_at(p)?;
    }
    let name = name(p)?;
    let ty = if matches!(p.peekt(), Some(T::Symbol("::"))) {
        kw_coloncolon(p)?;
        typ(p)
    } else {
        None
    };
    if is_paren {
        kw_rp(p)?;
    }
    Some(TypVarBinding(name, ty, is_at))
}

fn typ_var_bindings<'t>(p: &mut P<'t>) -> Vec<TypVarBinding<'t>> {
    many(p, "typ_var_bindings", typ_var_binding)
}

fn simple_typ_var_bindings<'t>(p: &mut P<'t>) -> Vec<TypVarBinding<'t>> {
    let vars = typ_var_bindings(p);
    for v in vars.iter() {
        if v.2 {
            p.raise(Serror::NotSimpleTypeVarBinding(v.0 .0 .1))
        }
    }
    vars
}

fn row_label<'t>(p: &mut P<'t>) -> Option<(Label<'t>, Typ<'t>)> {
    let l = match p.peek() {
        (Some(T::Lower(n) | T::String(n) | T::RawString(n)), s) => Label(S(n, s)),
        _ => return None,
    };
    kw_coloncolon(p)?;
    let t = typ(p)?;
    Some((l, t))
}

fn row<'t>(p: &mut P<'t>) -> Option<Row<'t>> {
    let c = sep(p, "row", kw_comma, row_label);

    let x = if matches!(p.peekt(), Some(T::Symbol("|"))) {
        kw_pipe(p)?;
        typ(p).map(|x| b!(x))
    } else {
        None
    };

    Some(Row(c, x))
}

#[derive(Debug, Eq, PartialEq)]
enum ExprOp<'t> {
    Op(QOp<'t>),
    Infix(Expr<'t>),
    App,
}

fn expr_op<'t>(p: &mut P<'t>) -> Option<ExprOp<'t>> {
    choice!(
        p: "expr_op",
        |p| { Some(ExprOp::Op(qop(p)?)) },
        |p: &mut P<'t>| {
            kw_tick(p)?;
            let e = expr(p)?;
            kw_tick(p)?;
            Some(ExprOp::Infix(e))
        },
        |p: &mut P<'t>| {
            expr_atom(p)?;
            Some(ExprOp::App)
        },
        |_| None::<ExprOp<'t>>
    )
}

fn expr_fop<'t>(t: &ExprOp<'t>) -> Prec {
    use Prec::*;
    match t {
        // NOTE[et]: With more information we can get the correct precedences
        ExprOp::Op(_) => R(1),
        ExprOp::Infix(_) => L(2),
        ExprOp::App => L(3),
    }
}

fn expr_mrg<'t>(p: &mut P<'t>, op: ExprOp<'t>, lhs: Expr<'t>, rhs: Expr<'t>) -> Expr<'t> {
    match op {
        ExprOp::Op(op) => Expr::Op(b!(lhs), op, b!(rhs)),
        ExprOp::Infix(op) => Expr::Infix(b!(lhs), b!(op), b!(rhs)),
        ExprOp::App => Expr::App(b!(lhs), b!(rhs)),
    }
}

fn expr<'t>(p: &mut P<'t>) -> Option<Expr<'t>> {
    let lhs = expr_atom(p)?;
    let e = pratt_expr(p, lhs, Prec::zero())?;
    Some(if matches!(p.peekt(), Some(T::Symbol("::"))) {
        kw_coloncolon(p)?;
        let t = typ(p)?;
        Expr::Typed(b!(e), t)
    } else {
        e
    })
}

fn expr_where<'t>(p: &mut P<'t>) -> Option<Expr<'t>> {
    let e = expr(p)?;
    if matches!(p.peekt(), Some(T::Where)) {
        let start = p.span();
        kw_where(p)?;
        if matches!(p.peekt(), Some(T::LayBegin)) {
            kw_begin(p)?;
        }
        let b = sep(p, "where-bindings", kw_sep, let_binding);
        if matches!(p.peekt(), Some(T::LayEnd)) {
            kw_end(p)?;
        }
        if matches!(p.peekt(), Some(T::LaySep)) {
            kw_sep(p)?;
        }
        Some(Expr::Where(start, b!(e), b))
    } else {
        Some(e)
    }
}

fn pratt_expr<'t>(p: &mut P<'t>, mut lhs: Expr<'t>, prec: usize) -> Option<Expr<'t>> {
    while let Some(outer_lookahead) = (|p: &mut P<'t>| {
        let op = expr_op(&mut p.fork())?;
        if expr_fop(&op).prec() >= prec {
            if !matches!(op, ExprOp::App) {
                let _ = expr_op(p)?;
            }
            Some(op)
        } else {
            None
        }
    })(p)
    {
        // Make the check a peek
        let mut rhs = expr_atom(p)?;
        while let Some(next) = (|p: &mut P<'t>| {
            let op = expr_op(&mut p.fork())?;
            expr_fop(&op).next(expr_fop(&outer_lookahead).prec())
        })(p)
        {
            let i = p.i;
            rhs = pratt_expr(p, rhs, next)?;
            assert_ne!(i, p.i, "STUCK EXPR!");
        }
        lhs = expr_mrg(p, outer_lookahead, lhs, rhs);
    }
    Some(lhs)
}

fn expr_atom<'t>(p: &mut P<'t>) -> Option<Expr<'t>> {
    let e = choice!(p: "expr_atom" ,
        |p: &mut P<'t>| {
            kw_minus(p)?;
            Some(Expr::Negate(b!(expr_atom(p)?)))
        },
        |p: &mut P<'t>| {
            Some(Expr::Boolean(boolean(p)?))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_if(p)?;
            let a = b!(expr(p)?);
            kw_then(p)?;
            let b = b!(expr(p)?);
            kw_else(p)?;
            let c = b!(expr(p)?);
            Some(Expr::IfThenElse(start, a, b, c))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_let(p)?;
            // Handle inline ones?
            if matches!(p.peekt(), Some(T::LayBegin)) {
                kw_begin(p)?;
            }
            let b = sep(p, "let-bindings", kw_sep, let_binding);
            if matches!(p.peekt(), Some(T::LayEnd)) {
                kw_end(p)?;
            }
            if matches!(p.peekt(), Some(T::LaySep)) {
                kw_sep(p)?;
            }
            kw_in(p)?;
            let e = b!(expr(p)?);
            Some(Expr::Let(start, b, e))
        },
        |p: &mut P<'t>| {
            kw_ado(p)?;
            if matches!(p.peekt(), Some(T::LayBegin)) {
                kw_begin(p)?;
            }
            let ds = sep(p, "ado-block", kw_sep, do_statement);
            if matches!(p.peekt(), Some(T::LayEnd)) {
                kw_end(p)?;
            }
            if matches!(p.peekt(), Some(T::LaySep)) {
                kw_sep(p)?;
            }
            let e = expr(p)?;
            Some(Expr::Ado(ds, b!(e)))
        },
        |p: &mut P<'t>| {
            kw_do(p)?;
            if matches!(p.peekt(), Some(T::LayBegin)) {
                kw_begin(p)?;
            }
            let ds = sep(p, "do-block", kw_sep, do_statement);
            if matches!(p.peekt(), Some(T::LayEnd)) {
                kw_end(p)?;
            }
            if matches!(p.peekt(), Some(T::LaySep)) {
                kw_sep(p)?;
            }
            Some(Expr::Do(ds))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_backslash(p)?;
            let binders = many(p, "lambda binder", binder_no_type);
            kw_right_arrow(p)?;
            let body = b!(expr(p)?);
            Some(Expr::Lambda(start, binders, body))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_case(p)?;
            let xs = sep(p, "case expr", kw_sep, expr);
            kw_of(p)?;
            kw_begin(p)?;
            let branches = sep(p, "case branch", kw_sep, case_branch);
            kw_end(p)?;
            kw_sep(p)?;
            Some(Expr::Case(start, xs, branches))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_underscore(p)?;
            Some(Expr::Section(start))
        },
        |p: &mut P<'t>| {
            Some(Expr::Hole(hole(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Ident(qname(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Constructor(qproper(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Symbol(qsymbol(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Char(char(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Str(string(p)?))
        },
        |p: &mut P<'t>| {
            Some(Expr::Number(number(p)?))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_ls(p)?;
            let inner = sep(p, "array expr", kw_comma, expr);
            kw_rs(p)?;
            let end = p.span();
            Some(Expr::Array(start, inner, end))
        },
        |p: &mut P<'t>| {
            let start = p.span();
            kw_lb(p)?;
            let inner = sep(p, "record expr", kw_comma, record_label);
            kw_rb(p)?;
            let end = p.span();
            Some(Expr::Record(start, inner, end))
        },
        |p: &mut P<'t>| {
            kw_lp(p)?;
            let inner = expr(p)?;
            kw_rp(p)?;
            Some(Expr::Paren(b!(inner)))
        },
    );
    let mut e = if let Some(e) = e {
        e
    } else {
        return p.raise_("Invalid expression");
    };

    if let Some(labels) = choice!(p: "expr_access" ,
        |p: &mut P<'t>| {
            kw_dot(p)?;
            Some(sep(p, "expr_access", kw_dot, label))
        },
        |_| None::<Vec<Label<'t>>>)
    {
        e = Expr::Access(b!(e), labels);
    }

    if let Some(x) = choice!(p: "expr_access" ,
        // NOTE: This isn't as good as it can be - we can know the user intends to do
        // this if we see a `=` in side the record
            record_updates,
        |_| None::<Vec<RecordUpdate<'t>>>)
    {
        e = Expr::Update(b!(e), x);
    }

    while let Some(labels) = choice!(p: "expr_vta" ,
        |p: &mut P<'t>| {
            kw_at(p)?;
            Some(typ(p)?)
        },
        |_| None::<Typ<'t>>)
    {
        e = Expr::Vta(b!(e), labels);
    }
    Some(e)
}

fn do_statement<'t>(p: &mut P<'t>) -> Option<DoStmt<'t>> {
    choice!(p: "do_statement",
        |p: &mut P<'t>| {
            let b = binder(p)?;
            kw_left_arrow(p)?;
            let e = expr(p)?;
            Some(DoStmt::Stmt(Some(b), e))
        },
        |p: &mut P<'t>| {
            let e = expr(p)?;
            Some(DoStmt::Stmt(None, e))
        },
        |p: &mut P<'t>| {
            kw_let(p)?;
            // Handle inline ones?
            if matches!(p.peekt(), Some(T::LayBegin)) {
                kw_begin(p)?;
            }
            let b = sep(p, "let-bindings", kw_sep, let_binding);
            if matches!(p.peekt(), Some(T::LayEnd)) {
                kw_end(p)?;
            }
            Some(DoStmt::Let(b))
        },
    )
}

fn let_binding<'t>(p: &mut P<'t>) -> Option<LetBinding<'t>> {
    choice!(p: "let_binding" ,
        |p: &mut P<'t>| {
            let b = binder(p)?;
            kw_eq(p)?;
            let e = expr_where(p)?;
            Some(LetBinding::Pattern(b, e))
        },
        |p: &mut P<'t>| {
            let n = name(p)?;
            let bs = many(p, "let-binder", binder_atom);
            let decl = guarded_decl(p)?;
            Some(LetBinding::Name(n, bs, decl))
        },
        |p: &mut P<'t>| {
            let n = name(p)?;
            kw_coloncolon(p)?;
            let t = typ(p)?;
            Some(LetBinding::Sig(n, t))
        },
    )
}

fn binder<'t>(p: &mut P<'t>) -> Option<Binder<'t>> {
    let b = binder_no_type(p)?;
    if matches!(p.peekt(), Some(T::Symbol("::"))) {
        kw_coloncolon(p)?;
        let t = typ(p)?;
        Some(Binder::Typed(b!(b), t))
    } else {
        Some(b)
    }
}

fn binder_no_type<'t>(p: &mut P<'t>) -> Option<Binder<'t>> {
    let bs = many(p, "binder_no_type", binder_atom);
    match Binder::toConstructor(bs) {
        Ok(a) => Some(a),
        Err(e) => p.raise_(e),
    }
}

fn binder_atom<'t>(p: &mut P<'t>) -> Option<Binder<'t>> {
    choice!(p: "binder_atom",
        |p: &mut P<'t>| {
            let start = p.span();
            kw_underscore(p)?;
            Some(Binder::Wildcard(start))
        },
        |p: &mut P<'t>| {
            let n = name(p)?;
            kw_at(p)?;
            let t = binder_atom(p)?;
            Some(Binder::Named(n, b!(t)))
        },
        |p: &mut P<'t>| {
            Some(Binder::Var(name(p)?))
        },
        |p: &mut P<'t>| {
            Some(Binder::Constructor(qproper(p)?))
        },
        |p: &mut P<'t>| {
            Some(Binder::Boolean(boolean(p)?))
        },
        |p: &mut P<'t>| {
            Some(Binder::Char(char(p)?))
        },
        |p: &mut P<'t>| {
            Some(Binder::Str(string(p)?))
        },
        |p: &mut P<'t>| {
            kw_minus(p)?;
            Some(Binder::Number(true, number(p)?))
        },
        |p: &mut P<'t>| {
            Some(Binder::Number(false, number(p)?))
        },
        |p: &mut P<'t>| {
            kw_ls(p)?;
            let bs = sep(p, "array binder", kw_comma, binder);
            kw_rs(p)?;
            Some(Binder::Array(bs))
        },
        |p: &mut P<'t>| {
            kw_lb(p)?;
            let bs = sep(p, "record binder", kw_comma, record_binder);
            kw_rb(p)?;
            Some(Binder::Record(bs))
        },
        |p: &mut P<'t>| {
            kw_lp(p)?;
            let b = b!(binder(p)?);
            kw_rp(p)?;
            Some(Binder::Paren(b))
        },
    )
}

fn record_binder<'t>(p: &mut P<'t>) -> Option<RecordLabelBinder<'t>> {
    choice!(p: "record_label",
        |p: &mut P<'t>| {
            let f = label(p)?;
            p.next();
            kw_colon(p)?;
            let e = binder(p)?;
            Some(RecordLabelBinder::Field(f, e))
        },
        |p: &mut P<'t>| {
            Some(RecordLabelBinder::Pun(name(p)?))
        },
    )
}

fn guarded_decl<'t>(p: &mut P<'t>) -> Option<GuardedExpr<'t>> {
    if matches!(p.peekt(), Some(T::Symbol("="))) {
        kw_eq(p)?;
        Some(GuardedExpr::Unconditional(expr_where(p)?))
    } else {
        Some(GuardedExpr::Guarded(many(
            p,
            "guardDeclExpr",
            guarded_decl_expr,
        )))
    }
}

fn guarded_decl_expr<'t>(p: &mut P<'t>) -> Option<(Vec<Guard<'t>>, Expr<'t>)> {
    if !matches!(p.peekt(), Some(T::Symbol("|"))) {
        return None;
    }
    let gs = guard(p)?;
    kw_eq(p)?;
    let e = expr_where(p)?;
    Some((gs, e))
}

fn guard<'t>(p: &mut P<'t>) -> Option<Vec<Guard<'t>>> {
    kw_pipe(p)?;
    Some(sep(p, "guard", kw_comma, guard_statement))
}

fn guard_statement<'t>(p: &mut P<'t>) -> Option<Guard<'t>> {
    choice!(p: "guard statements",
        |p: &mut P<'t>| {
            let b = binder(p)?;
            kw_left_arrow(p)?;
            let e = expr(p)?;
            Some(Guard::Binder(b, e))
        },
        |p: &mut P<'t>| {
            Some(Guard::Expr(expr(p)?))
        },
    )
}

fn record_label<'t>(p: &mut P<'t>) -> Option<RecordLabelExpr<'t>> {
    choice!(p: "record_label",
        |p: &mut P<'t>| {
            let f = label(p)?;
            p.next();
            kw_colon(p)?;
            let e = expr(p)?;
            Some(RecordLabelExpr::Field(f, e))
        },
        |p: &mut P<'t>| {
            Some(RecordLabelExpr::Pun(name(p)?))
        },
    )
}

fn record_updates<'t>(p: &mut P<'t>) -> Option<Vec<RecordUpdate<'t>>> {
    kw_lb(p)?;
    let updates = sep(p, "record_updates", kw_comma, record_update);
    kw_rb(p)?;
    Some(updates)
}

fn record_update<'t>(p: &mut P<'t>) -> Option<RecordUpdate<'t>> {
    let f = label(p)?;
    p.next();
    kw_eq(p)?;

    choice!(p: "record_label",
        |p: &mut P<'t>| {
            Some(RecordUpdate::Leaf(f.clone(), expr(p)?))
        },
        |p: &mut P<'t>| {
            Some(RecordUpdate::Branch(f.clone(), record_updates(p)?))
        },
    )
}

fn case_branch<'t>(p: &mut P<'t>) -> Option<CaseBranch<'t>> {
    let bs = sep(p, "case_branch", kw_comma, binder_no_type);
    let x = guarded_case(p)?;
    Some(CaseBranch(bs, x))
}

fn guarded_case<'t>(p: &mut P<'t>) -> Option<GuardedExpr<'t>> {
    if matches!(p.peekt(), Some(T::RightArrow)) {
        kw_right_arrow(p)?;
        Some(GuardedExpr::Unconditional(expr_where(p)?))
    } else {
        kw_pipe(p)?;
        Some(GuardedExpr::Guarded(sep(
            p,
            "guarded_case_expr",
            kw_sep,
            guarded_case_expr,
        )))
    }
}

fn guarded_case_expr<'t>(p: &mut P<'t>) -> Option<(Vec<Guard<'t>>, Expr<'t>)> {
    let gs = sep(p, "guard", kw_comma, guard_statement);
    kw_right_arrow(p)?;
    let e = expr_where(p)?;
    Some((gs, e))
}

#[derive(Clone, Debug)]
enum Serror<'s> {
    Unexpected(Span, Option<Token<'s>>, &'static str),
    NotSimpleTypeVarBinding(Span),
    NotAConstraint(Span),
}

#[derive(Clone, Debug)]
struct P<'s> {
    fi: usize,
    i: usize,
    tokens: &'s Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>,
    errors: Vec<Serror<'s>>,
    panic: bool,
    steps: std::cell::RefCell<usize>,
}

impl<'s> P<'s> {
    fn new(fi: usize, tokens: &'s Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>) -> Self {
        Self {
            fi,
            i: 0,
            steps: 0.into(),
            panic: false,
            errors: Vec::new(),
            tokens,
        }
    }

    fn check_loop(&self) {
        *self.steps.borrow_mut() += 1;
        if *self.steps.borrow() > 1000 {
            panic!("Found loop in parser, {}\n{:?}", self.i, self.tokens)
        }
    }

    fn peek_(&self) -> (Option<Token<'s>>, Span) {
        self.check_loop();
        (self.tokens.get(self.i).and_then(|x| x.0.ok()), self.span())
    }

    fn peek(&self) -> (Option<Token<'s>>, Span) {
        if self.panic {
            (None, self.span())
        } else {
            self.peek_()
        }
    }

    fn peek2t(&self) -> (Option<Token<'s>>, Option<Token<'s>>) {
        self.check_loop();
        if self.panic {
            (None, None)
        } else {
            (
                self.tokens.get(self.i).and_then(|x| x.0.ok()),
                self.tokens.get(self.i + 1).and_then(|x| x.0.ok()),
            )
        }
    }

    fn peekt(&self) -> Option<Token<'s>> {
        if self.panic {
            None
        } else {
            self.peek_().0
        }
    }

    fn eof(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn next(&mut self) -> (Option<Token<'s>>, Span) {
        if self.panic {
            return (None, self.span());
        }
        let out = self.peek_();
        self.skip();
        out
    }

    fn span(&self) -> Span {
        let s = self.tokens.get(self.i).or_else(|| self.tokens.last());
        Span::Known(
            s.map(|x| x.1.start).unwrap_or(0),
            s.map(|x| x.1.end).unwrap_or(0),
            self.fi,
        )
    }

    fn fork(&mut self) -> Self {
        self.check_loop();
        self.clone()
    }

    fn skip(&mut self) {
        *self.steps.borrow_mut() = 0;
        self.i += 1;
        // NOTE: For now we skipp some of the tokens in this parser
        while matches!(
            self.peek_().0,
            Some(Token::LineComment(_) | Token::BlockComment(_))
        ) {
            self.i += 1;
        }
    }

    fn recover<F>(&mut self, f: F) -> Result<(), ()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if !self.panic {
            return Ok(());
        }
        while let (Some(x), _) = self.peek_() {
            if f(x) {
                self.panic = false;
                return Ok(());
            } else {
                self.skip()
            }
        }
        Err(())
    }

    fn iff<F>(&mut self, f: F, err: &'static str) -> Option<()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if let (Some(x), _) = self.peek() {
            if f(x) {
                self.next();
                return Some(());
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn expect<F>(&mut self, f: F, err: &'static str) -> Option<()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if let (Some(x), _) = self.peek() {
            if f(x) {
                self.next();
                return Some(());
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn expect_<F, A>(&mut self, f: F, err: &'static str) -> Option<A>
    where
        F: Fn(Token<'s>) -> Option<A>,
    {
        if let (Some(x), _) = self.peek() {
            if let Some(a) = f(x) {
                self.next();
                return Some(a);
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn raise_<A>(&mut self, err: &'static str) -> Option<A> {
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn raise(&mut self, err: Serror<'s>) {
        self.errors.push(err)
    }

    fn panic(&mut self) {
        self.panic = true;
    }

    fn raise_and_panic(&mut self, err: Serror<'s>) {
        self.raise(err);
        self.panic();
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    macro_rules! gen_parser {
        ($a:ident, $p:ident) => {
            pub fn $a<'t>(src: &'t str) -> String {
                use super::*;
                use crate::lexer;
                use std::io::BufWriter;

                let l = lexer::lex(&src);
                let mut p = P::new(0, &l);

                let mut buf = BufWriter::new(Vec::new());
                $p(&mut p).show(0, &mut buf).unwrap();
                let inner = buf.into_inner().map_err(|x| format!("{:?}", x)).unwrap();

                format!(
                    "{} of {}\n===\n{}===\n{}",
                    p.i,
                    p.tokens.len(),
                    String::from_utf8(inner)
                        .map_err(|x| format!("{:?}", x))
                        .unwrap(),
                    p.errors
                        .iter()
                        .map(|x| format!("{:?}", x))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
        };
    }

    gen_parser!(p_header, header);

    #[test]
    fn empty_string() {
        assert_snapshot!(p_header(""));
    }

    #[test]
    fn normal_definition() {
        assert_snapshot!(p_header("module A (a, b, c) where"));
    }

    #[test]
    fn everything_header() {
        assert_snapshot!(p_header(
            r#"
module A.B.C (a, class B, C, D(..), E(F, G), 
 (+), type (+), module H) where

import A
import A as A
import A.B.C as A.C
import A.B.C (a, class B, 
 C, D(..), E(F, G), (+), type (+))
import A.B.C hiding (foo)
        "#
        ));
    }

    gen_parser!(p_import, import_decl);

    #[test]
    fn simple_joe_import() {
        assert_snapshot!(p_import("import Joe"))
    }

    #[test]
    fn simple_import() {
        assert_snapshot!(p_import("import A as A"))
    }

    #[test]
    fn import_with_newlines() {
        assert_snapshot!(p_import("import A (foo\n , bar\n , baz)"))
    }

    gen_parser!(p_typ, typ);

    #[test]
    fn typ_empty_row() {
        assert_snapshot!(p_typ("()"))
    }

    #[test]
    fn typ_row_kind_ambiguity_row() {
        assert_snapshot!(p_typ("( a :: A )"))
    }

    #[test]
    fn typ_row_kind_ambiguity_typ() {
        assert_snapshot!(p_typ("A :: Type"))
    }

    #[test]
    fn typ_higher_kinded() {
        assert_snapshot!(p_typ("Foo bar biz"))
    }

    #[test]
    fn typ_a_signature() {
        assert_snapshot!(p_typ("forall a. Monoid a => a -> a -> a"))
    }

    #[test]
    fn typ_b_signature() {
        assert_snapshot!(p_typ("forall a b . Foo a b => Bar a b => b -> b"))
    }

    #[test]
    fn typ_b_simple_signature() {
        assert_snapshot!(p_typ("Foo => Bar => b -> b"))
    }

    #[test]
    fn typ_c_signature() {
        assert_snapshot!(p_typ(
            "
     forall out pkA pkB propsB rowB propsA rowA select
   . GetPk (C pkA N) propsA
  => ReflectArray pkA
  => HasMagicMapping rowA rowA
  => AllColumns rowA select
  => HasMagicSelect propsA rowA select select out
  => GetPk (C pkB N) propsB
  => ReflectArray pkB
  => HasMagicMapping rowB rowB
  => Table propsA rowA
  -> Table propsB rowB
  -> (Record out -> Record rowB)
  -> Sql Unit"
        ))
    }

    #[test]
    fn typ_operators() {
        assert_snapshot!(p_typ("Array $ Maybe $ Maybe $ Int"))
    }

    gen_parser!(p_typ_var_binding, typ_var_binding);

    #[test]
    fn typ_var_bindings_a() {
        assert_snapshot!(p_typ_var_binding("a"))
    }

    #[test]
    fn typ_var_bindings_a_at() {
        assert_snapshot!(p_typ_var_binding("@a"))
    }

    #[test]
    fn typ_var_bindings_a_at_paren() {
        assert_snapshot!(p_typ_var_binding("( @a )"))
    }

    #[test]
    fn typ_var_bindings_a_at_paren_kind() {
        assert_snapshot!(p_typ_var_binding("( @a :: Kind )"))
    }

    gen_parser!(p_expr, expr);

    #[test]
    fn expr_simple() {
        assert_snapshot!(p_expr("1 + 1"))
    }

    #[test]
    fn expr_tick() {
        assert_snapshot!(p_expr("a `b` c"))
    }

    #[test]
    fn expr_messy() {
        assert_snapshot!(p_expr(
            "(1 + 1) * 2 + foo @A `a + b` A.B.C.d A.B.+ q :: Int"
        ))
    }

    #[test]
    fn expr_record() {
        assert_snapshot!(p_expr("{ a, b: 1 }"))
    }

    #[test]
    fn expr_record_construct() {
        assert_snapshot!(p_expr("foo { a : 1 }"))
    }

    #[test]
    fn expr_record_update() {
        assert_snapshot!(p_expr("foo { a = 1 }"))
    }

    #[test]
    fn expr_record_update_full() {
        assert_snapshot!(p_expr("foo { a = 1, b = { c = 1 }, d = { e: 1 } }"))
    }

    #[test]
    fn expr_if() {
        assert_snapshot!(p_expr("if a == 2 then b else c"))
    }

    #[test]
    fn expr_let() {
        assert_snapshot!(p_expr(
            r"
            let
                x = 1
            in 2
    "
        ))
    }

    #[test]
    fn expr_let_multiple() {
        assert_snapshot!(p_expr(
            r"
            let
                x = 1
                x = 1
                x = 1
            in 2
    "
        ))
    }

    #[test]
    fn expr_let_inline() {
        assert_snapshot!(p_expr(r"let x = 1 in 2"))
    }

    #[test]
    fn expr_case() {
        assert_snapshot!(p_expr(
            r"
        case 1 + 1 of
            2 -> foo
            3 | Just _ <- foo bar -> baz
                    where
                        baz = 3
        "
        ))
    }

    #[test]
    fn expr_do() {
        assert_snapshot!(p_expr(
            r"
        do
            a <- f 2
            b <- g a
            fazz 1 2
            let 
                a = 1
            pure (b + a)
        "
        ))
    }

    #[test]
    fn expr_ado() {
        assert_snapshot!(p_expr(
            r"
        ado
            a <- f 2
            b <- g a
            fazz 1 2
            let 
                a = 1
            in b + a
        "
        ))
    }
}
