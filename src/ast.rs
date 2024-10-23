use std::io::Write;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Span {
    lo: usize,
    hi: usize,
    fi: usize,
}

impl Span {
    fn merge(self, other: Self) -> Self {
        assert_eq!(other.fi, self.fi, "Cannot merge spans files!");
        Self {
            lo: self.lo.min(other.lo),
            hi: self.hi.min(other.hi),
            fi: self.fi,
        }
    }
}


pub trait Ast {
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()>;
}

impl Ast for str {
    fn show(&self, _: usize, _: &mut impl Write) -> ::std::io::Result<()> {
        Ok(())
    }
}

impl Ast for &str {
    fn show(&self, _: usize, _: &mut impl Write) -> ::std::io::Result<()> {
        Ok(())
    }
}

impl<T> Ast for S<T>
where
    T: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        writeln!(w, "{:indent$}{}..{}", "", self.1.lo, self.1.hi, indent = indent)?;
        self.0.show(indent + 1, w)
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
}

impl<A> Ast for Box<A>
where
    A: Ast,
{
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        (&*self as &A).show(indent, w)
    }
}

impl Ast for bool {
    fn show(&self, _: usize, _: &mut impl Write) -> ::std::io::Result<()> {
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub T, pub Span);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QProperName<'t>(pub S<Vec<Qual<'t>>>, pub ProperName<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QName<'t>(pub S<Vec<Qual<'t>>>, pub Name<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QSymbol<'t>(pub S<Vec<Qual<'t>>>, pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QOp<'t>(pub S<Vec<Qual<'t>>>, pub S<&'t str>);

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
pub struct Module<'t>(pub Header<'t>, pub Vec<Decl<'t>>);

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
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl<'t> {
    DataKind(ProperName<'t>, Typ<'t>),
    Data(
        ProperName<'t>,
        Vec<Name<'t>>,
        Vec<(ProperName<'t>, Vec<Typ<'t>>)>,
    ),

    TypeKind(ProperName<'t>, Typ<'t>),
    Type(ProperName<'t>, Vec<Name<'t>>, Typ<'t>),

    NewTypeKind(ProperName<'t>, Typ<'t>),
    NewType(ProperName<'t>, Vec<Name<'t>>, ProperName<'t>, Typ<'t>),

    Class(ClassHead<'t>, Vec<ClassMember<'t>>),
    Instance(InstHead<'t>, Vec<InstBinding<'t>>),
    Derive(InstHead<'t>, bool, Vec<InstBinding<'t>>),
    Foreign(ProperName<'t>, bool, Vec<InstBinding<'t>>),
    ForeignData(InstHead<'t>, bool, Vec<InstBinding<'t>>),

    Sig(Name<'t>, Typ<'t>),
    Def(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
    // TODO: Fixity, role,
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstBinding<'t> {
    Sig(Name<'t>, Typ<'t>),
    Def(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstHead<'t>(Vec<Constraint<'t>>, QProperName<'t>, Vec<Typ<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassMember<'t>(Name<'t>, Typ<'t>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassHead<'t>(
    Vec<Constraint<'t>>,
    ProperName<'t>,
    Vec<Name<'t>>,
    Vec<FunDep<'t>>,
);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint<'t>(QProperName<'t>, Vec<Typ<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunDep<'t>(Vec<Name<'t>>, Vec<Name<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Typ<'t> {
    Wildcard,
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

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypVarBinding<'t>(Name<'t>, Option<Typ<'t>>, bool);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row<'t>(Vec<(Label<'t>, Typ<'t>)>, Option<Box<Typ<'t>>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Binder<'t> {
    Wildcard,
    Var(Name<'t>),
    Named(Name<'t>, Box<Binder<'t>>),
    Constructor(QProperName<'t>, Vec<Binder<'t>>),
    Boolean(Boolean<'t>),
    Char(Char<'t>),
    Str(Str<'t>),
    Number(Number<'t>),
    Array(Vec<Binder<'t>>),
    Record(Vec<(Label<'t>, Binder<'t>)>),
    Parens(Box<Binder<'t>>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GuardedExpr<'t> {
    Unconditional(ExprWhere<'t>),
    Guarded(Vec<Binder<'t>>, ExprWhere<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExprWhere<'t>(Expr<'t>, Vec<LetBinding<'t>>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LetBinding<'t> {
    Sig(Name<'t>, Typ<'t>),
    Name(Name<'t>, Vec<Binder<'t>>, GuardedExpr<'t>),
    Pattern(Binder<'t>, ExprWhere<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr<'t> {
    Number(Number<'t>),
    Var(QName<'t>),
}
