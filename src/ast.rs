#![allow(unused)]

use std::io::Write;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Span {
    Known(usize, usize, usize),
    Zero,
}

impl Span {
    pub fn zero() -> Self {
        Span::Zero
    }

    pub fn merge(self, other: Self) -> Self {
        use Span::*;
        match (self, other) {
            (Known(a_lo, a_hi, a_fi), Known(b_lo, b_hi, b_fi)) => {
                assert_eq!(a_fi, b_fi);
                Known(a_lo.min(b_lo), a_hi.max(b_hi), a_fi)
            }
            (a @ Known(_, _, _), Zero) | (Zero, a @ Known(_, _, _)) => a,
            _ => self,
        }
    }

    pub fn lo(&self) -> usize {
        match self {
            Span::Known(l, _, _) => *l,
            Span::Zero => 0,
        }
    }

    pub fn hi(&self) -> usize {
        match self {
            Span::Known(_, h, _) => *h,
            Span::Zero => 0,
        }
    }
}

pub trait Ast {
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()>;

    fn span(&self) -> Span;
}

macro_rules! ast {
    ($t:ty) => {
        impl Ast for $t {
            fn show(&self, _: usize, _: &mut impl Write) -> ::std::io::Result<()> {
                Ok(())
            }

            fn span(&self) -> Span {
                Span::zero()
            }
        }
    };
}

ast!(str);
ast!(&str);
ast!(bool);

impl Ast for Span {
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        writeln!(
            w,
            "{:indent$}{}..{}",
            "",
            self.lo(),
            self.hi(),
            indent = indent
        )
    }

    fn span(&self) -> Span {
        *self
    }
}

impl<T> Ast for S<T>
where
    T: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        self.1.show(indent, w)?;
        self.0.show(indent + 1, w)
    }

    fn span(&self) -> Span {
        self.1
    }
}

impl<T> Ast for Vec<T>
where
    T: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        for i in self.iter() {
            i.show(indent + 1, w)?;
        }
        Ok(())
    }

    fn span(&self) -> Span {
        self.iter().fold(Span::zero(), |a, b| a.merge(b.span()))
    }
}

impl<A, B> Ast for (A, B)
where
    A: Ast,
    B: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        writeln!(w, "{:indent$}(", "", indent = indent)?;
        self.0.show(indent + 1, w)?;
        self.1.show(indent + 1, w)?;
        writeln!(w, "{:indent$})", "", indent = indent)
    }

    fn span(&self) -> Span {
        self.0.span().merge(self.1.span())
    }
}

impl<A, B> Ast for Result<A, B>
where
    A: Ast,
    B: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        match self {
            Ok(a) => a.show(indent, w),
            Err(a) => a.show(indent, w),
        }
    }

    fn span(&self) -> Span {
        match self {
            Ok(a) => a.span(),
            Err(a) => a.span(),
        }
    }
}

impl<A> Ast for Option<A>
where
    A: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        match self {
            Some(x) => x.show(indent, w),
            None => writeln!(w, "{:indent$}NULL", "", indent = indent),
        }
    }

    fn span(&self) -> Span {
        match self {
            Some(a) => a.span(),
            None => Span::zero(),
        }
    }
}

impl<A> Ast for Box<A>
where
    A: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        (&**self as &A).show(indent, w)
    }

    fn span(&self) -> Span {
        (&**self as &A).span()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub T, pub Span);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QProperName<'t>(pub Option<Qual<'t>>, pub ProperName<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QName<'t>(pub Option<Qual<'t>>, pub Name<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QSymbol<'t>(pub Option<Qual<'t>>, pub Symbol<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QOp<'t>(pub Option<Qual<'t>>, pub Op<'t>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qual<'t>(pub S<&'t str>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ProperName<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Hole<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Op<'t>(pub S<&'t str>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Str<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Int<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Char<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Number<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct HexInt<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Boolean<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label<'t>(pub S<&'t str>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Header<'t>(
    pub QProperName<'t>,
    pub Vec<Export<'t>>,
    pub Vec<ImportDecl<'t>>,
);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module<'t>(pub Option<Header<'t>>, pub Vec<Decl<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataMember<'t> {
    All,
    Some(Vec<ProperName<'t>>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Export<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypSymbol(Symbol<'t>),
    TypDat(ProperName<'t>, DataMember<'t>),
    Class(ProperName<'t>),
    Module(QProperName<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMember<'t>),
    TypSymbol(Symbol<'t>),
    Class(ProperName<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ImportDecl<'t> {
    As(QProperName<'t>, QProperName<'t>),
    Multiple(QProperName<'t>, Vec<Import<'t>>),
    Hiding(QProperName<'t>, Vec<Import<'t>>),
    Bulk(QProperName<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl<'t> {
    DataKind(ProperName<'t>, Typ<'t>),
    Data(
        ProperName<'t>,
        Vec<TypVarBinding<'t>>,
        Vec<(ProperName<'t>, Vec<Typ<'t>>)>,
    ),

    TypeKind(ProperName<'t>, Typ<'t>),
    Type(ProperName<'t>, Vec<TypVarBinding<'t>>, Typ<'t>),

    NewTypeKind(ProperName<'t>, Typ<'t>),
    NewType(
        ProperName<'t>,
        Vec<TypVarBinding<'t>>,
        ProperName<'t>,
        Typ<'t>,
    ),

    ClassKind(ProperName<'t>, Typ<'t>),
    Class(
        Vec<Constraint<'t>>,
        ProperName<'t>,
        Vec<TypVarBinding<'t>>,
        Vec<FunDep<'t>>,
        Vec<ClassMember<'t>>,
    ),

    Instance(bool, InstHead<'t>, Vec<InstBinding<'t>>),
    Derive(bool, InstHead<'t>),

    Foreign(Name<'t>, Typ<'t>),
    ForeignData(ProperName<'t>, Typ<'t>),

    Role(ProperName<'t>, Vec<S<Role>>),

    Fixity(S<FixitySide>, Number<'t>, Expr<'t>, Op<'t>),
    FixityTyp(S<FixitySide>, Number<'t>, Typ<'t>, Op<'t>),

    Sig(Name<'t>, Typ<'t>),
    Def(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
}

#[derive(purring_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FixitySide {
    L,
    R,
    C,
}

#[derive(purring_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstBinding<'t> {
    Sig(Name<'t>, Typ<'t>),
    Def(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstHead<'t>(
    pub Vec<Constraint<'t>>,
    pub QProperName<'t>,
    pub Vec<Typ<'t>>,
);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassMember<'t>(pub Name<'t>, pub Typ<'t>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint<'t>(pub QProperName<'t>, pub Vec<Typ<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunDep<'t>(pub Vec<Name<'t>>, pub Vec<Name<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Typ<'t> {
    Wildcard(Span),
    Var(Name<'t>),
    Constructor(QProperName<'t>),
    Symbol(QSymbol<'t>),
    Str(Str<'t>),
    Int(Int<'t>),
    Hole(Hole<'t>),
    Record(S<Row<'t>>),
    Row(S<Row<'t>>),

    Forall(Vec<TypVarBinding<'t>>, Box<Typ<'t>>),
    Kinded(Box<Typ<'t>>, Box<Typ<'t>>),
    Arr(Box<Typ<'t>>, Box<Typ<'t>>),
    Op(Box<Typ<'t>>, QOp<'t>, Box<Typ<'t>>),
    Constrained(Constraint<'t>, Box<Typ<'t>>),
    App(Box<Typ<'t>>, Box<Typ<'t>>),
}

impl<'t> Typ<'t> {
    pub fn cast_to_constraint(self) -> Option<Constraint<'t>> {
        fn inner<'t>(a: Typ<'t>, mut args: Vec<Typ<'t>>) -> Option<Constraint<'t>> {
            match a {
                Typ::Symbol(_)
                | Typ::Str(_)
                | Typ::Int(_)
                | Typ::Hole(_)
                | Typ::Record(_)
                | Typ::Var(_)
                | Typ::Wildcard(_)
                | Typ::Row(_)
                | Typ::Forall(_, _)
                | Typ::Kinded(_, _)
                | Typ::Op(_, _, _)
                | Typ::Constrained(_, _)
                | Typ::Arr(_, _) => None,

                Typ::Constructor(n) => Some(Constraint(n, args)),
                Typ::App(l, r) => {
                    args.insert(0, *r);
                    inner(*l, args)
                }
            }
        }
        inner(self, Vec::new())
    }
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypVarBinding<'t>(pub Name<'t>, pub Option<Typ<'t>>, pub bool);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row<'t>(pub Vec<(Label<'t>, Typ<'t>)>, pub Option<Box<Typ<'t>>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Binder<'t> {
    Typed(Box<Binder<'t>>, Typ<'t>),

    App(Box<Binder<'t>>, Box<Binder<'t>>),
    // TODO
    Op(Box<Binder<'t>>, QOp<'t>, Box<Binder<'t>>),

    Wildcard(Span),
    Var(Name<'t>),
    Named(Name<'t>, Box<Binder<'t>>),
    Constructor(QProperName<'t>),
    Boolean(Boolean<'t>),
    Char(Char<'t>),
    Str(Str<'t>),
    Number(bool, Number<'t>),
    Array(Vec<Binder<'t>>),
    Record(Vec<RecordLabelBinder<'t>>),
    Paren(Box<Binder<'t>>),
}

impl<'t> Binder<'t> {
    pub fn to_constructor(bs: Vec<Binder<'t>>) -> Result<Binder<'t>, &'static str> {
        Ok(match (bs.first().cloned(), bs.len()) {
            (None, 0) => return Err("Empty binder is not allowed"),
            (Some(a), 1) => a,
            (Some(a@Binder::Constructor(_)), _) => {
                let mut out = a;
                for b in bs.into_iter().skip(1) {
                    out = Binder::App(Box::new(out), Box::new(b));
                }
                out
            }
            (_, _) => return Err("For this to be a constructor application the first binder needs to be a constructor"),
        })
    }
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GuardedExpr<'t> {
    Unconditional(Expr<'t>),
    Guarded(Vec<(Vec<Guard<'t>>, Expr<'t>)>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Guard<'t> {
    Expr(Expr<'t>),
    Binder(Binder<'t>, Expr<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LetBinding<'t> {
    Sig(Name<'t>, Typ<'t>),
    Name(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
    Pattern(Binder<'t>, Expr<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr<'t> {
    Typed(Box<Expr<'t>>, Typ<'t>),
    Op(Box<Expr<'t>>, QOp<'t>, Box<Expr<'t>>),
    Infix(Box<Expr<'t>>, Box<Expr<'t>>, Box<Expr<'t>>),
    Negate(Box<Expr<'t>>),
    App(Box<Expr<'t>>, Box<Expr<'t>>),
    Vta(Box<Expr<'t>>, Typ<'t>),
    IfThenElse(Span, Box<Expr<'t>>, Box<Expr<'t>>, Box<Expr<'t>>),
    Do(Option<Qual<'t>>, Vec<DoStmt<'t>>),
    Ado(Option<Qual<'t>>, Vec<DoStmt<'t>>, Box<Expr<'t>>),
    Lambda(Span, Vec<Binder<'t>>, Box<Expr<'t>>),
    Let(Span, Vec<LetBinding<'t>>, Box<Expr<'t>>),
    Where(Span, Box<Expr<'t>>, Vec<LetBinding<'t>>),

    Case(Span, Vec<Expr<'t>>, Vec<CaseBranch<'t>>),

    Array(Span, Vec<Expr<'t>>, Span),
    Record(Span, Vec<RecordLabelExpr<'t>>, Span),
    Update(Box<Expr<'t>>, Vec<RecordUpdate<'t>>),
    Access(Box<Expr<'t>>, Vec<Label<'t>>),

    Section(Span),
    Hole(Hole<'t>),
    Ident(QName<'t>),
    Constructor(QProperName<'t>),
    Symbol(QSymbol<'t>),
    Boolean(Boolean<'t>),
    Char(Char<'t>),
    Str(Str<'t>),
    Number(Number<'t>),
    HexInt(HexInt<'t>),
    Paren(Box<Expr<'t>>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordLabelExpr<'t> {
    Pun(Name<'t>),
    Field(Label<'t>, Expr<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordLabelBinder<'t> {
    Pun(Name<'t>),
    Field(Label<'t>, Binder<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordUpdate<'t> {
    Leaf(Label<'t>, Expr<'t>),
    Branch(Label<'t>, Vec<RecordUpdate<'t>>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct CaseBranch<'t>(pub Vec<Binder<'t>>, pub GuardedExpr<'t>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DoStmt<'t> {
    Stmt(Option<Binder<'t>>, Expr<'t>),
    Let(Vec<LetBinding<'t>>),
}
