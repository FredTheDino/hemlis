#![allow(unused)]

use std::io::Write;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Fi(pub usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Span {
    Known(Fi, (usize, usize), (usize, usize)),
    Zero,
}

// NOTE: I've assumed we don't have hash-collisions - given that a hit has a ~2^64 chance of
// happening - I judge we're more likely to be hindered by the limit on 2^32 lines in a
// document before we start to see collisions.
// Since most of the strings are small - we are close to optimal if it is cryptographically
// strong and follows the `Avalance Effect`.
// https://www.geeksforgeeks.org/avalanche-effect-in-cryptography/
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Ud(pub usize);

impl Span {
    pub fn zero() -> Self {
        Span::Zero
    }

    pub fn contains(self, (l, c): (usize, usize)) -> bool {
        self.contains_(l, c)
    }

    pub fn contains_(self, l: usize, c: usize) -> bool {
        self.lo() <= (l, c) && (l, c) < self.hi()
    }

    pub fn merge(self, other: Self) -> Self {
        use Span::*;
        match (self, other) {
            (
                Known(a_fi, a_lo, a_hi),
                Known(b_fi, b_lo, b_hi),
            ) => {
                assert_eq!(a_fi, b_fi);
                let lo = a_lo.min(b_lo);
                let hi = a_hi.min(b_hi);
                Known(a_fi, lo, hi)
            }
            (a @ Known(..), Zero) | (Zero, a @ Known( .. )) => a,
            _ => self,
        }
    }

    pub fn fi(&self) -> Option<Fi> {
        match self {
            Span::Known(fi, ..) => Some(*fi),
            Span::Zero => None
        }
    }

    pub fn lo(&self) -> (usize, usize) {
        match self {
            Span::Known(_, lo, _) => *lo,
            Span::Zero => (0, 0),
        }
    }

    pub fn hi(&self) -> (usize, usize) {
        match self {
            Span::Known(_, _, hi) => *hi,
            Span::Zero => (0, 0),
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
            "{:indent$}{:?}->{:?}",
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

impl Ast for Ud {
    fn show(&self, _indent: usize, _w: &mut impl Write) -> ::std::io::Result<()> {
        Ok(())
    }

    fn span(&self) -> Span {
        Span::Zero
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub T, pub Span);

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QProperName(pub Option<Qual>, pub ProperName);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QName(pub Option<Qual>, pub Name);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QSymbol(pub Option<Qual>, pub Symbol);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QOp(pub Option<Qual>, pub Op);

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Qual(pub S<Ud>);

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProperName(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Name(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Hole(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Op(pub S<Ud>);

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Str(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Int(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Char(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Number(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct HexInt(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Boolean(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Label(pub S<Ud>);
#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MName(pub S<Ud>);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Header(
    pub MName,
    pub Option<Vec<Export>>,
    pub Vec<ImportDecl>,
);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module(pub Option<Header>, pub Vec<Decl>);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataMember {
    All,
    Some(Vec<ProperName>),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Export {
    Value(Name),
    Symbol(Symbol),
    Typ(ProperName),
    TypSymbol(Symbol),
    TypDat(ProperName, DataMember),
    Class(ProperName),
    Module(MName),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import {
    Value(Name),
    Symbol(Symbol),
    Typ(ProperName),
    TypDat(ProperName, DataMember),
    TypSymbol(Symbol),
    Class(ProperName),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportDecl {
    pub from: MName,
    pub hiding: Vec<Import>,
    pub names: Vec<Import>,
    pub to: Option<MName>,
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    DataKind(ProperName, Typ),
    Data(
        ProperName,
        Vec<TypVarBinding>,
        Vec<(ProperName, Vec<Typ>)>,
    ),

    TypeKind(ProperName, Typ),
    Type(ProperName, Vec<TypVarBinding>, Typ),

    NewTypeKind(ProperName, Typ),
    NewType(
        ProperName,
        Vec<TypVarBinding>,
        ProperName,
        Typ,
    ),

    ClassKind(ProperName, Typ),
    Class(
        Option<Vec<Constraint>>,
        ProperName,
        Vec<TypVarBinding>,
        Option<Vec<FunDep>>,
        Vec<ClassMember>,
    ),

    Instance(bool, InstHead, Vec<InstBinding>),
    Derive(bool, InstHead),

    Foreign(Name, Typ),
    ForeignData(ProperName, Typ),

    Role(ProperName, Vec<S<Role>>),

    Fixity(S<FixitySide>, Number, Expr, Op),
    FixityTyp(S<FixitySide>, Number, Typ, Op),

    Sig(Name, Typ),
    Def(Name, Vec<Binder>, GuardedExpr),
}

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FixitySide {
    L,
    R,
    C,
}

#[derive(hemlis_macros::Ast, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstBinding {
    Sig(Name, Typ),
    Def(Name, Vec<Binder>, GuardedExpr),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstHead(
    pub Option<Vec<Constraint>>,
    pub QProperName,
    pub Vec<Typ>,
);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassMember(pub Name, pub Typ);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint(pub QProperName, pub Vec<Typ>);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunDep(pub Vec<Name>, pub Vec<Name>);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Typ {
    Wildcard(Span),
    Var(Name),
    Constructor(QProperName),
    Symbol(QSymbol),
    Str(Str),
    Int(Int),
    Hole(Hole),
    Record(S<Row>),
    Row(S<Row>),

    Forall(Vec<TypVarBinding>, Box<Typ>),
    Kinded(Box<Typ>, Box<Typ>),
    Arr(Box<Typ>, Box<Typ>),
    Op(Box<Typ>, QOp, Box<Typ>),
    Constrained(Constraint, Box<Typ>),
    App(Box<Typ>, Box<Typ>),
}

impl Typ {
    pub fn cast_to_constraint(self) -> Option<Constraint> {
        println!("{:?}", self);
        fn inner(a: Typ, mut args: Vec<Typ>) -> Option<Constraint> {
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

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypVarBinding(pub Name, pub Option<Typ>, pub bool);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row(pub Vec<(Label, Typ)>, pub Option<Box<Typ>>);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Binder {
    Typed(Box<Binder>, Typ),

    App(Box<Binder>, Box<Binder>),
    // TODO
    Op(Box<Binder>, QOp, Box<Binder>),

    Wildcard(Span),
    Var(Name),
    Named(Name, Box<Binder>),
    Constructor(QProperName),
    Boolean(Boolean),
    Char(Char),
    Str(Str),
    Number(bool, Number),
    Array(Vec<Binder>),
    Record(Vec<RecordLabelBinder>),
    Paren(Box<Binder>),
}

impl Binder {
    pub fn to_constructor(bs: Vec<Binder>) -> Result<Binder, &'static str> {
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

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GuardedExpr {
    Unconditional(Expr),
    Guarded(Vec<(Vec<Guard>, Expr)>),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Guard {
    Expr(Expr),
    Binder(Binder, Expr),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LetBinding {
    Sig(Name, Typ),
    Name(Name, Vec<Binder>, GuardedExpr),
    Pattern(Binder, Expr),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Typed(Box<Expr>, Typ),
    Op(Box<Expr>, QOp, Box<Expr>),
    Infix(Box<Expr>, Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Vta(Box<Expr>, Typ),
    IfThenElse(Span, Box<Expr>, Box<Expr>, Box<Expr>),
    Do(Option<Qual>, Vec<DoStmt>),
    Ado(Option<Qual>, Vec<DoStmt>, Box<Expr>),
    Lambda(Span, Vec<Binder>, Box<Expr>),
    Let(Span, Vec<LetBinding>, Box<Expr>),
    Where(Span, Box<Expr>, Vec<LetBinding>),

    Case(Span, Vec<Expr>, Vec<CaseBranch>),

    Array(Span, Vec<Expr>, Span),
    Record(Span, Vec<RecordLabelExpr>, Span),
    Update(Box<Expr>, Vec<RecordUpdate>),
    Access(Box<Expr>, Vec<Label>),

    Section(Span),
    Hole(Hole),
    Ident(QName),
    Constructor(QProperName),
    Symbol(QSymbol),
    Boolean(Boolean),
    Char(Char),
    Str(Str),
    Number(Number),
    HexInt(HexInt),
    Paren(Box<Expr>),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordLabelExpr {
    Pun(Name),
    Field(Label, Expr),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordLabelBinder {
    Pun(Name),
    Field(Label, Binder),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordUpdate {
    Leaf(Label, Expr),
    Branch(Label, Vec<RecordUpdate>),
}

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct CaseBranch(pub Vec<Binder>, pub GuardedExpr);

#[derive(hemlis_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DoStmt {
    Stmt(Option<Binder>, Expr),
    Let(Vec<LetBinding>),
}
