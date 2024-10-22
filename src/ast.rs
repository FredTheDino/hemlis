

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub usize, pub T, pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QProperName<'t>(pub S<Vec<Qual<'t>>>, pub ProperName<'t>);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QName<'t>(pub S<Vec<Qual<'t>>>, pub Name<'t>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qual<'t>(pub &'t str);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ProperName<'t>(pub S<&'t str>);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name<'t>(pub S<&'t str>);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol<'t>(pub S<&'t str>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Header<'t>(
    pub QProperName<'t>,
    pub Vec<Export<'t>>,
    pub Vec<Import<'t>>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataMember<'t> {
    All,
    Some(Vec<ProperName<'t>>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Export<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMember<'t>),
    Class(ProperName<'t>),
    Module(QProperName<'t>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ImportPiece<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMember<'t>),
    Class(ProperName<'t>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import<'t> {
    As(QProperName<'t>, QProperName<'t>),
    Multiple(QProperName<'t>, Vec<ImportPiece<'t>>),
    Hiding(QProperName<'t>, Vec<ImportPiece<'t>>),
}
