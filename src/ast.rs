pub struct Qual<'t>(pub &'t str);

pub struct QProperName<'t>(pub Vec<Qual<'t>>, pub ProperName<'t>);
pub struct QName<'t>(pub Vec<Qual<'t>>, pub Name<'t>);

pub struct ProperName<'t>(pub &'t str);
pub struct Name<'t>(pub &'t str);
pub struct Symbol<'t>(pub &'t str);

pub struct Header<'t>(
    pub QProperName<'t>,
    pub Vec<Export<'t>>,
    pub Vec<(ProperName<'t>, Vec<Import<'t>>)>,
);

pub enum DataMembers<'t> {
    All,
    Some(Vec<ProperName<'t>>),
}

pub enum Export<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMembers<'t>),
    Class(ProperName<'t>),
    Module(QProperName<'t>),
}

pub enum Import<'t> {
    Value(Name<'t>),
    Symbol(Symbol<'t>),
    Typ(ProperName<'t>),
    TypDat(ProperName<'t>, DataMembers<'t>),
    Class(ProperName<'t>),
}
