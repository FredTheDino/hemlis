use std::io::Write;


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

impl<T> Ast for S<T> where T: Ast {
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        writeln!(w, "{:indent$}{}..{}", "", self.0, self.2, indent=indent)?;
        self.1.show(indent + 1, w)
    }
}

impl<T> Ast for Vec<T> where T: Ast {
    fn show(&self, indent: usize, w: &mut impl Write) -> ::std::io::Result<()> {
        for i in self.iter() {
            i.show(indent + 1, w)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub usize, pub T, pub usize);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QProperName<'t>(pub S<Vec<Qual<'t>>>, pub ProperName<'t>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct QName<'t>(pub S<Vec<Qual<'t>>>, pub Name<'t>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qual<'t>(pub &'t str);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ProperName<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name<'t>(pub S<&'t str>);
#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol<'t>(pub S<&'t str>);

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Header<'t>(
    pub QProperName<'t>,
    pub Vec<Export<'t>>,
    pub Vec<Import<'t>>,
);

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
pub enum ImportPiece<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMember<'t>),
    TypSymbol(Symbol<'t>),
    Class(ProperName<'t>),
}

#[derive(purring_macros::Ast, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import<'t> {
    As(QProperName<'t>, QProperName<'t>),
    Multiple(QProperName<'t>, Vec<ImportPiece<'t>>),
    Hiding(QProperName<'t>, Vec<ImportPiece<'t>>),
}
