use crate::ast;
use dashmap::DashMap;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Visibility {
    Private((usize, usize)),
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub Scope, pub ast::Ud, pub ast::Ud, pub Visibility);

impl Name {
    pub fn is(self, s: Scope, u: ast::Ud) -> bool {
        let Name(ss, _, uu, _) = self;
        ss == s && uu == u
    }

    pub fn show<'a>(&'a self, f: &'a impl Fn(&'a ast::Ud) -> String) -> String {
        let Name(ss, mm, uu, _) = self;
        format!("{:?} {}.{}", ss, f(mm), f(uu))
    }

    fn scope(&self) -> Scope {
        self.0
    }

    pub fn module(&self) -> ast::Ud {
        self.1
    }

    pub fn name(&self) -> ast::Ud {
        self.2
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scope {
    Kind,
    Type,
    Class,
    Term,
    Module,
    Namespace,
}

use Scope::*;

#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum Export {
    ConstructorsSome(Name, Vec<Name>),
    ConstructorsAll(Name, Vec<Name>),
    Just(Name),
}

impl Export {
    pub fn show<'a>(&'a self, f: &'a impl Fn(&'a ast::Ud) -> String) -> String {
        match self {
            Export::ConstructorsSome(name, vec) => format!(
                "ConstructorsSome {} {:?}",
                name.show::<'a>(f),
                vec.iter().map(|n| n.show::<'a>(f)).collect::<Vec<_>>()
            ),
            Export::ConstructorsAll(name, vec) => format!(
                "ConstructorsAll {} {:?}",
                name.show(f),
                vec.iter().map(|n| n.show(f)).collect::<Vec<_>>()
            ),
            Export::Just(name) => format!("Just {}", name.show(f)),
        }
    }

    pub fn contains(&self, name: Name) -> bool {
        match self {
            Export::ConstructorsSome(n, xs) | Export::ConstructorsAll(n, xs) => {
                *n == name || xs.iter().any(|x| *x == name)
            }
            Export::Just(n) => *n == name,
        }
    }

    pub fn to_names(&self) -> Vec<Name> {
        match self {
            Export::ConstructorsAll(n, xs) | Export::ConstructorsSome(n, xs) => {
                [vec![*n], xs.to_vec()].concat()
            }
            Export::Just(n) => vec![*n],
        }
    }
}

// Deliberately not `Copy` so you always use it right
#[derive(Debug)]
pub struct Sf(usize);

#[derive(Debug)]
pub enum NRerrors {
    Unknown(Scope, Option<ast::Ud>, ast::Ud, ast::Span),
    NameConflict(BTreeSet<Name>, ast::Span),
    MultipleDefinitions(Name, (usize, usize), (usize, usize)),
    NotAConstructor(Name, ast::ProperName),
    NoConstructors(Name, ast::Span),
    NotExportedOrDoesNotExist(ast::Ud, Scope, ast::Ud, ast::Span),
    CannotImportSelf(ast::Span),
    CouldNotFindImport(ast::Ud, ast::Span),
}

pub type Pos = (usize, usize);

#[derive(Debug)]
pub struct N<'s> {
    pub me: ast::Ud,

    global_exports: &'s DashMap<ast::Ud, Vec<Export>>,
    pub global_usages: BTreeSet<(Name, ast::Span)>,

    pub usages: BTreeMap<Name, BTreeSet<ast::Span>>,

    pub errors: Vec<NRerrors>,

    pub resolved: BTreeMap<(Pos, Pos), Name>,
    pub exports: Vec<Export>,

    constructors: BTreeMap<Name, BTreeSet<Name>>,

    pub defines: BTreeMap<Name, ast::Span>,
    locals: Vec<Name>,
    pub imports: BTreeMap<Option<ast::Ud>, BTreeMap<ast::Ud, Vec<Export>>>,
}

impl<'s> N<'s> {
    pub fn new(me: ast::Ud, global_exports: &'s DashMap<ast::Ud, Vec<Export>>) -> Self {
        Self {
            me,
            usages: BTreeMap::new(),

            global_exports,
            global_usages: BTreeSet::new(),

            errors: Vec::new(),
            resolved: BTreeMap::new(),
            exports: Vec::new(),
            constructors: BTreeMap::new(),
            imports: BTreeMap::new(),
            defines: BTreeMap::new(),
            locals: Vec::new(),
        }
    }

    fn push(&mut self) -> Sf {
        Sf(self.locals.len())
    }

    fn pop(&mut self, Sf(l): Sf) {
        self.locals.truncate(l)
    }

    fn def(&mut self, s: ast::Span, name: Name, ignore_error: bool) {
        match self.defines.entry(name) {
            std::collections::btree_map::Entry::Vacant(v) => {
                v.insert(s);
            }
            std::collections::btree_map::Entry::Occupied(v) => {
                if !ignore_error {
                    self.errors.push(NRerrors::MultipleDefinitions(
                        *v.key(),
                        v.get().lo(),
                        s.lo(),
                    ));
                }
            }
        }
        self.resolved.insert((s.lo(), s.hi()), name);
        self.usages.entry(name).or_default().insert(s);
    }

    fn def_global(&mut self, scope: Scope, s: ast::S<ast::Ud>, is_redecl: bool) {
        let name = Name(scope, self.me, s.0, Visibility::Public);
        self.def(s.1, name, is_redecl)
    }

    fn def_local(&mut self, scope: Scope, u: ast::Ud, s: ast::Span) {
        let name = Name(scope, self.me, u, Visibility::Private(s.lo()));
        self.locals.push(name);

        self.def(s, name, false)
    }

    fn resolve(&mut self, scope: Scope, m: Option<ast::Ud>, n: ast::S<ast::Ud>) -> Option<Name> {
        let s = n.1;
        let n = n.0;
        let unique_matches = self.resolve_inner(scope, m, n);

        for name in unique_matches.iter().copied() {
            self.resolved.insert((s.lo(), s.hi()), name);
            if name.1 == self.me {
                self.usages.entry(name).or_default().insert(s);
            } else {
                self.global_usages.insert((name, s));
            }
        }

        match unique_matches.len() {
            0 => self.errors.push(NRerrors::Unknown(scope, m, n, s)),
            1 => (),
            _ => self
                .errors
                .push(NRerrors::NameConflict(unique_matches.clone(), s)),
        }
        unique_matches.first().copied()
    }

    fn resolveq(&mut self, scope: Scope, m: Option<ast::Qual>, n: ast::S<ast::Ud>) -> Option<Name> {
        let m = m.map(|x| {
            self.resolve(Namespace, None, x.0);
            x.0 .0
        });
        self.resolve(scope, m, n)
    }

    fn find_local(&self, ss: Scope, n: ast::Ud) -> Option<Name> {
        self.locals
            .iter()
            .rfind(|x| x.name() == n && x.scope() == ss)
            .copied()
    }

    // For `A.B.C.foo` does `A.B.C` resolve to the module - or does it resolve to `foo`?
    fn resolve_inner(&self, ss: Scope, m: Option<ast::Ud>, n: ast::Ud) -> BTreeSet<Name> {
        // NOTE: There's a strict hirarcy in purs where names are resolved
        //  1. Locals - trumps everything
        //  2. Globals from module
        //  3. Imports
        //  It doesn't matter if the import is into the global namespace - it's fine to shadow them
        //  according to purs.
        [
            if m.is_none() {
                self.find_local(ss, n).into_iter().collect::<BTreeSet<_>>()
            } else {
                BTreeSet::new()
            },
            if self
                .defines
                .contains_key(&Name(ss, self.me, n, Visibility::Public))
                && m.is_none()
            {
                [Name(ss, self.me, n, Visibility::Public)].into()
            } else {
                BTreeSet::new()
            },
            self.imports
                .get(&m)
                .iter()
                .flat_map(|x| {
                    x.values()
                        .flatten()
                        .flat_map(|i| i.to_names())
                        .find(|p| p.is(ss, n))
                })
                .collect(),
        ]
        .into_iter()
        .find(|x| !x.is_empty())
        .unwrap_or_default()
    }

    fn export(&mut self, ex: &ast::Export) {
        // TODO: Resolve usages here so I can goto definition on things
        use Export::*;
        match ex {
            ast::Export::Value(v) => {
                if let Some(n) = self.resolve(Term, None, v.0) {
                    self.exports.push(Just(n))
                }
            }
            ast::Export::Symbol(v) => {
                if let Some(n) = self.resolve(Term, None, v.0) {
                    self.exports.push(Just(n))
                }
            }
            ast::Export::Typ(v) => {
                if let Some(n) = self.resolve(Type, None, v.0) {
                    self.exports.push(Just(n))
                }
            }
            ast::Export::TypSymbol(v) => {
                if let Some(n) = self.resolve(Type, None, v.0) {
                    self.exports.push(Just(n))
                }
            }
            ast::Export::Class(v) => {
                if let Some(n) = self.resolve(Class, None, v.0) {
                    self.exports.push(Just(n))
                }
            }
            ast::Export::TypDat(v, ds) => {
                self.resolve(Type, None, v.0);
                let x = Name(Type, self.me, v.0 .0, Visibility::Public);
                let ms = match self.constructors.get(&x) {
                    None => {
                        self.errors.push(NRerrors::NoConstructors(x, v.0 .1));
                        return;
                    }
                    Some(ms) => ms.clone(),
                };
                let out = match ds {
                    ast::DataMember::All => ConstructorsAll(x, ms.iter().copied().collect()),
                    ast::DataMember::Some(ns) => ConstructorsSome(
                        x,
                        ns.iter()
                            .filter_map(|m| match ms.iter().find(|a| a.2 == m.0 .0) {
                                Some(a) => {
                                    self.resolve(a.0, None, m.0);
                                    Some(*a)
                                }
                                None => {
                                    self.errors.push(NRerrors::NotAConstructor(x, *m));
                                    None
                                }
                            })
                            .collect(),
                    ),
                };
                self.exports.push(out);
            }

            ast::Export::Module(v) if v.0 .0 == self.me => {
                self.export_self();
            }
            ast::Export::Module(v) => {
                if let Some(ns) = self.imports.get(&Some(v.0 .0)).cloned() {
                    self.resolve(Namespace, None, v.0);
                    self.exports
                        .append(&mut ns.values().flatten().cloned().collect());
                } else {
                    let name = Name(Scope::Module, v.0 .0, v.0 .0, Visibility::Public);
                    self.resolved.insert((v.0 .1.lo(), v.0 .1.hi()), name);
                    // Module exports export everything that's ever imported from a module -
                    // right?
                    let imports = self
                        .imports
                        .values()
                        .flatten()
                        .filter(|(x, _)| **x == v.0 .0)
                        .collect::<Vec<_>>();
                    if imports.is_empty() {
                        self.errors.push(NRerrors::Unknown(
                            Scope::Module,
                            Some(v.0 .0),
                            v.0 .0,
                            v.0 .1,
                        ));
                    } else {
                        for (_, is) in imports.into_iter() {
                            self.exports.append(&mut is.clone());
                        }
                    }
                }
            }
        }
    }

    fn export_self(&mut self) {
        let cs: BTreeSet<_> = self.constructors.values().flatten().collect();
        for name @ Name(s, m, _, v) in self.defines.keys() {
            if *v != Visibility::Public {
                continue;
            }
            if *m != self.me {
                continue;
            }
            if !matches!(s, Scope::Class | Scope::Term | Scope::Type) {
                continue;
            }
            if let Some(co) = self.constructors.get(name) {
                self.exports.push(Export::ConstructorsAll(
                    *name,
                    co.iter().copied().collect::<Vec<_>>(),
                ))
            } else if !cs.contains(name) {
                self.exports.push(Export::Just(*name))
            }
        }
    }

    fn import(
        &mut self,
        ast::ImportDecl {
            from,
            hiding,
            names,
            to,
        }: &ast::ImportDecl,
    ) {
        // NOTE: I've decided the export isn't a usage - it's annoying to see references that
        // aren't really used.
        let name = Name(Module, from.0 .0, from.0 .0, Visibility::Public);
        self.global_usages.insert((name, from.0 .1));
        self.resolved.insert((from.0 .1.lo(), from.0 .1.hi()), name);
        let import_name = to.map(|x| x.0 .0);
        self.imports
            .entry(import_name)
            .or_default()
            .entry(from.0 .0)
            .or_default();
        if let Some(b) = to {
            self.def_global(Namespace, b.0, true);
        }
        if from.0 .0 == self.me {
            self.errors.push(NRerrors::CannotImportSelf(from.0 .1));
            return;
        }
        if !self.global_exports.contains_key(&from.0 .0) {
            self.errors
                .push(NRerrors::CouldNotFindImport(from.0 .0, from.0 .1));
            return;
        }
        let exports: Vec<Export> = self.global_exports.get(&from.0 .0).unwrap().value().clone();

        let valid: BTreeMap<_, _> = exports
            .iter()
            .cloned()
            .flat_map(|x| x.to_names())
            .map(|name @ Name(s, _, u, _)| ((s, u), name))
            .collect();
        if names.is_empty() {
            let hiding = hiding
                .iter()
                .filter_map(|x| {
                    let (s, u) = match x {
                        ast::Import::Value(x) => (Term, x.0),
                        ast::Import::Symbol(x) => (Term, x.0),
                        ast::Import::Typ(x) => (Type, x.0),
                        // NOTE[et]: It's bug compatible with purs to not hide the members
                        ast::Import::TypDat(x, ast::DataMember::All) => (Type, x.0),
                        // These are not references in hiding since they don't do anything.
                        ast::Import::TypDat(x, ast::DataMember::Some(_)) => (Type, x.0),
                        ast::Import::TypSymbol(x) => (Type, x.0),
                        ast::Import::Class(x) => (Class, x.0),
                    };
                    if let Some(n) = valid.get(&(s, u.0)) {
                        // Opinionatedly not adding imports to usages
                        self.resolved.insert((u.1.lo(), u.1.hi()), *n);
                        // NOTE[et]: This is bug-compatible with purs
                        if matches!(x, ast::Import::TypDat(_, ast::DataMember::Some(_))) {
                            return None;
                        }
                        Some((s, u.0))
                    } else {
                        self.errors
                            .push(NRerrors::NotExportedOrDoesNotExist(from.0 .0, s, u.0, u.1));
                        None
                    }
                })
                .collect::<BTreeSet<_>>();
            let entry = self
                .imports
                .get_mut(&import_name)
                .expect("Checked earlier")
                .get_mut(&from.0 .0)
                .expect("Checked earlier");
            entry.append(
                &mut exports
                    .into_iter()
                    .filter(|x| {
                        x.to_names()
                            .iter()
                            .all(|Name(s, _, n, _)| !hiding.contains(&(*s, *n)))
                    })
                    .collect(),
            );
        } else {
            // TODO: I'm gonna need a test-suite for this
            let mut to_export = names
                .iter()
                .filter_map(|i| self.import_part(i, from.0 .0, &exports))
                .collect();
            self.imports
                .get_mut(&import_name)
                .expect("Checked earlier")
                .get_mut(&from.0 .0)
                .expect("Checked earlier")
                .append(&mut to_export);
        }
    }

    fn import_part(&mut self, i: &ast::Import, from: ast::Ud, valid: &[Export]) -> Option<Export> {
        let mut export_as = |scope: Scope, x: ast::Ud, s: ast::Span| -> Option<Export> {
            if let out @ Some(_) = valid.iter().find_map(|n| match n {
                out @ Export::Just(name) if name.is(scope, x) => Some(out.clone()),
                Export::ConstructorsSome(name, _) | Export::ConstructorsAll(name, _)
                    if name.is(scope, x) =>
                {
                    Some(Export::Just(*name))
                }
                _ => None,
            }) {
                out
            } else {
                self.errors
                    .push(NRerrors::NotExportedOrDoesNotExist(from, scope, x, s));
                None
            }
        };

        Some(match i {
            ast::Import::Value(ast::Name(ast::S(x, s)))
            | ast::Import::Symbol(ast::Symbol(ast::S(x, s))) => export_as(Term, *x, *s)?,
            ast::Import::TypSymbol(ast::Symbol(ast::S(x, s)))
            | ast::Import::Typ(ast::ProperName(ast::S(x, s))) => export_as(Type, *x, *s)?,
            ast::Import::Class(ast::ProperName(ast::S(x, s))) => export_as(Class, *x, *s)?,
            ast::Import::TypDat(x, ast::DataMember::All) => {
                if let Some(out) = valid.iter().find_map(|n| match n {
                    out
                    @ (Export::ConstructorsSome(name, _) | Export::ConstructorsAll(name, _))
                        if name.is(Type, x.0 .0) =>
                    {
                        Some(out)
                    }
                    _ => None,
                }) {
                    out.clone()
                } else {
                    self.errors.push(NRerrors::NotExportedOrDoesNotExist(
                        from, Type, x.0 .0, x.0 .1,
                    ));
                    return None;
                }
            }
            ast::Import::TypDat(x, ast::DataMember::Some(cs)) => {
                if let Some((name, es)) = valid.iter().find_map(|n| match n {
                    Export::ConstructorsSome(name, cs) | Export::ConstructorsAll(name, cs)
                        if name.is(Type, x.0 .0) =>
                    {
                        Some((name, cs))
                    }
                    _ => None,
                }) {
                    let es = es
                        .iter()
                        .map(|n @ Name(_, _, x, _)| (x, n))
                        .collect::<BTreeMap<_, _>>();
                    let cs = cs
                        .iter()
                        .filter_map(|n| {
                            if let Some(xx) = es.get(&n.0 .0) {
                                Some(**xx)
                            } else {
                                self.errors.push(NRerrors::NotExportedOrDoesNotExist(
                                    from, Term, n.0 .0, n.0 .1,
                                ));
                                None
                            }
                        })
                        .collect();
                    Export::ConstructorsSome(*name, cs)
                } else {
                    self.errors.push(NRerrors::NotExportedOrDoesNotExist(
                        from, Type, x.0 .0, x.0 .1,
                    ));
                    return None;
                }
            }
        })
    }

    // NOTE: This is needs to be split into two passes - one for the initial declarations and
    // one for inner declarations - since there is no order here. One could also push these
    // references first and check them later - saying where the same declaration is used in e.g
    // error messages.
    fn decl_first(&mut self, d: &ast::Decl, is_redecl: bool) {
        // I skipped references in Kinds for now - not because it's hard but because I
        // want a demo ASAP and it has little value.
        //
        // NOTE: There's a sneaky bug here, where multiple kind definitions don't cause an error. I
        // don't think this will be that annoying - but it requires more through analysis to
        // resolve. There's also the case with term-definitions where there can be guards - this
        // requires more sophisticated checking. (Or just returning None if it's a catch-all?)
        match d {
            ast::Decl::DataKind(d, _) => {
                self.def_global(Type, d.0, is_redecl);
            }
            ast::Decl::Data(d, _, cs) => {
                self.def_global(Type, d.0, is_redecl);
                let mut cons = BTreeSet::new();
                for c in cs {
                    self.def_global(Term, c.0 .0, false);
                    cons.insert(Name(Term, self.me, c.0 .0 .0, Visibility::Public));
                }
                self.constructors
                    .insert(Name(Type, self.me, d.0 .0, Visibility::Public), cons);
            }
            ast::Decl::TypeKind(d, _) => {
                self.def_global(Type, d.0, is_redecl);
            }
            ast::Decl::Type(d, _, _) => {
                self.def_global(Type, d.0, is_redecl);
                // Bug compatible with the Purs-compiler
                self.constructors.insert(
                    Name(Type, self.me, d.0 .0, Visibility::Public),
                    BTreeSet::new(),
                );
            }
            ast::Decl::NewTypeKind(d, _) => {
                self.def_global(Type, d.0, is_redecl);
            }
            ast::Decl::NewType(d, _, c, _) => {
                self.def_global(Type, d.0, is_redecl);
                self.def_global(Term, c.0, false);
                self.constructors.insert(
                    Name(Type, self.me, d.0 .0, Visibility::Public),
                    [Name(Term, self.me, c.0 .0, Visibility::Public)].into(),
                );
            }
            ast::Decl::ClassKind(d, _) => {
                self.def_global(Type, d.0, is_redecl);
            }
            ast::Decl::Class(_, d, _, _, mem) => {
                self.def_global(Class, d.0, is_redecl);
                for ast::ClassMember(name, _) in mem.iter() {
                    self.def_global(Term, name.0, false);
                }
            }
            ast::Decl::Foreign(d, _) => {
                self.def_global(Term, d.0, false);
            }
            ast::Decl::ForeignData(d, _) => {
                self.def_global(Type, d.0, false);
            }
            ast::Decl::Fixity(_, _, _, o) => {
                self.def_global(Term, o.0, false);
            }
            ast::Decl::FixityTyp(_, _, _, o) => {
                self.def_global(Type, o.0, false);
            }
            ast::Decl::Sig(d, _) => {
                self.def_global(Term, d.0, false);
            }
            ast::Decl::Def(d, _, _) => {
                self.def_global(Term, d.0, is_redecl);
            }
            ast::Decl::Instance(_, _, _) => (),
            ast::Decl::Derive(_, _) => (),
            ast::Decl::Role(_, _) => (),
        }
    }

    fn decl_body(&mut self, d: &ast::Decl) {
        // TODO: Kind signatures
        match d {
            ast::Decl::DataKind(_, _) => (),
            ast::Decl::Data(_, xs, cs) => {
                let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                for c in cs {
                    for t in c.1.iter() {
                        self.typ(t);
                    }
                }
                self.pop(sf);
            }
            ast::Decl::TypeKind(_, _) => (),
            ast::Decl::Type(_, xs, t) => {
                let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                self.typ(t);
                self.pop(sf);
            }
            ast::Decl::NewTypeKind(_, _) => (),
            ast::Decl::NewType(_, xs, _, t) => {
                let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                self.typ(t);
                self.pop(sf);
            }
            ast::Decl::ClassKind(_, _) => (),
            ast::Decl::Class(cs, _, xs, deps, mem) => {
                let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                for ast::FunDep(a, b) in deps.iter().flatten() {
                    for n in a.iter().chain(b.iter()) {
                        self.resolve(Type, None, n.0);
                    }
                }
                for c in cs.iter().flatten() {
                    self.constraint(c);
                }
                for ast::ClassMember(_, typ) in mem.iter() {
                    let sf = self.push();
                    self.typ(typ);
                    self.pop(sf);
                }
                self.pop(sf);
            }
            ast::Decl::Instance(_, head, bindings) => {
                let sf = self.push();
                let u = self.inst_head(head);
                let grouped = group_by(bindings.iter(), |d: &ast::InstBinding| d.ud());
                for bs in grouped.values() {
                    let sf = self.push();
                    for b in bs.iter() {
                        self.inst_binding(b, u);
                    }
                    self.pop(sf);
                }
                self.pop(sf);
            }
            ast::Decl::Derive(_, head) => {
                let sf = self.push();
                let _u = self.inst_head(head);
                self.pop(sf);
            }
            ast::Decl::Foreign(_, t) => {
                let sf = self.push();
                self.typ(t);
                self.pop(sf);
            }
            ast::Decl::ForeignData(_, _) => {}
            ast::Decl::Role(d, _) => {
                self.resolve(Type, None, d.0);
            }
            ast::Decl::Fixity(_, _, e, _) => {
                self.expr(e);
            }
            ast::Decl::FixityTyp(_, _, t, _) => {
                let sf = self.push();
                self.typ(t);
                self.pop(sf);
            }
            ast::Decl::Sig(_, t) => {
                // This is handled accross definitions since signatures need to have their
                // types available in the expression body.
                // let sf = self.push();
                self.typ(t);
                // self.pop(sf);
            }
            ast::Decl::Def(_, bs, e) => {
                let sf = self.push();
                for b in bs.iter() {
                    self.binder(b);
                }
                self.guarded_expr(e);
                self.pop(sf);
            }
        }
    }

    fn inst_head(&mut self, ast::InstHead(cs, d, ts): &ast::InstHead) -> Option<Name> {
        for t in ts.iter() {
            self.typ_define_vars(t);
        }
        for ast::Constraint(_, ts) in cs.iter().flatten() {
            for t in ts.iter() {
                self.typ_define_vars(t);
            }
        }
        let sf = self.push();
        for t in ts.iter() {
            self.typ(t);
        }
        self.pop(sf);
        for c in cs.iter().flatten() {
            self.constraint(c);
        }
        self.resolveq(Class, d.0, d.1 .0)
    }

    fn inst_binding(&mut self, b: &ast::InstBinding, u: Option<Name>) {
        // TODO: Check if these names actually are exported from whence they came
        let l = match b {
            ast::InstBinding::Sig(l, t) => {
                self.typ(t);
                l
            }
            ast::InstBinding::Def(l, binders, e) => {
                let sf = self.push();
                for b in binders.iter() {
                    self.binder(b);
                }
                self.guarded_expr(e);
                self.pop(sf);
                l
            }
        };
        if let Some(n) = u {
            let span = l.0 .1;
            self.resolved.insert(
                (span.lo(), span.hi()),
                Name(Term, n.module(), l.0 .0, Visibility::Public),
            );
        }
    }

    fn guarded_expr(&mut self, e: &ast::GuardedExpr) {
        match e {
            ast::GuardedExpr::Unconditional(e) => self.expr(e),
            ast::GuardedExpr::Guarded(es) => {
                for (gs, e) in es.iter() {
                    let sf = self.push();
                    for g in gs {
                        match g {
                            ast::Guard::Expr(e) => self.expr(e),
                            ast::Guard::Binder(b, e) => {
                                // NOTE[et]: We define things from the expression, so the flow of
                                // execution is different here.
                                self.expr(e);
                                self.binder(b);
                            }
                        }
                    }
                    self.expr(e);
                    self.pop(sf);
                }
            }
        }
    }

    fn expr(&mut self, e: &ast::Expr) {
        match e {
            ast::Expr::Typed(e, t) => {
                let sf = self.push();
                self.typ(t);
                self.expr(e);
                self.pop(sf);
            }
            ast::Expr::Op(a, o, b) => {
                self.expr(a);
                self.resolveq(Term, o.0, o.1 .0);
                self.expr(b);
            }
            ast::Expr::Infix(a, o, b) => {
                self.expr(a);
                self.expr(o);
                self.expr(b);
            }
            ast::Expr::Negate(e) => {
                self.expr(e);
            }
            ast::Expr::App(a, b) => {
                self.expr(a);
                self.expr(b);
            }
            ast::Expr::Vta(e, t) => {
                self.expr(e);
                let sf = self.push();
                self.typ(t);
                self.pop(sf);
            }
            ast::Expr::IfThenElse(_, a, tru, fal) => {
                self.expr(a);
                self.expr(tru);
                self.expr(fal);
            }
            ast::Expr::Do(qual, stmts) => {
                if let Some(qual) = qual {
                    self.resolve(Namespace, None, qual.0);
                }
                let sf = self.push();
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(Some(b), e) => {
                            // NOTE[et]: We define things from the expression, so the flow of
                            // execution is different here.
                            self.expr(e);
                            self.binder(b);
                        }
                        ast::DoStmt::Stmt(None, e) => {
                            self.expr(e);
                        }
                        ast::DoStmt::Let(ls) => {
                            self.let_binders(ls);
                        }
                    }
                }
                self.pop(sf);
            }
            ast::Expr::Ado(qual, stmts, ret) => {
                if let Some(qual) = qual {
                    self.resolve(Namespace, None, qual.0);
                }
                let sf = self.push();
                // NOTE[et]: We define things from the expression, so the flow of
                // execution is different here.
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(_, e) => {
                            self.expr(e);
                        }
                        ast::DoStmt::Let(_) => {}
                    }
                }
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(Some(b), _) => {
                            self.binder(b);
                        }
                        ast::DoStmt::Stmt(None, _) => {}
                        ast::DoStmt::Let(ls) => {
                            self.let_binders(ls);
                        }
                    }
                }
                self.expr(ret);
                self.pop(sf);
            }
            ast::Expr::Lambda(_, bs, e) => {
                let sf = self.push();
                for b in bs.iter() {
                    self.binder(b);
                }
                self.expr(e);
                self.pop(sf);
            }
            ast::Expr::Where(_, e, ls) | ast::Expr::Let(_, ls, e) => {
                let sf = self.push();
                self.let_binders(ls);
                self.expr(e);
                self.pop(sf);
            }
            ast::Expr::Case(_, es, cs) => {
                for e in es.iter() {
                    self.expr(e);
                }
                for ast::CaseBranch(bs, e) in cs.iter() {
                    let sf = self.push();
                    for b in bs.iter() {
                        self.binder(b);
                    }
                    self.guarded_expr(e);
                    self.pop(sf);
                }
            }
            ast::Expr::Array(_, es, _) => {
                for e in es.iter() {
                    self.expr(e);
                }
            }
            ast::Expr::Record(_, rs, _) => {
                for r in rs {
                    match r {
                        ast::RecordLabelExpr::Pun(l) => {
                            self.resolve(Term, None, l.0);
                        }
                        ast::RecordLabelExpr::Field(_, e) => {
                            self.expr(e);
                        }
                    }
                }
            }
            ast::Expr::Update(e, rs) => {
                self.expr(e);
                for r in rs.iter() {
                    self.record_update(r);
                }
            }
            ast::Expr::Access(e, _) => {
                self.expr(e);
            }
            ast::Expr::Section(_) => (),
            ast::Expr::Hole(_) => (),
            ast::Expr::Ident(v) => {
                self.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Constructor(v) => {
                self.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Symbol(v) => {
                self.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Boolean(_) => (),
            ast::Expr::Char(_) => (),
            ast::Expr::Str(_) => (),
            ast::Expr::Number(_) => (),
            ast::Expr::HexInt(_) => (),
            ast::Expr::Paren(e) => {
                self.expr(e);
            }
        }
    }

    fn record_update(&mut self, r: &ast::RecordUpdate) {
        match r {
            ast::RecordUpdate::Leaf(_, e) => {
                self.expr(e);
            }
            ast::RecordUpdate::Branch(_, rs) => {
                for r in rs.iter() {
                    self.record_update(r);
                }
            }
        }
    }

    fn let_binders(&mut self, ls: &[ast::LetBinding]) {
        let grouped = group_by(ls.iter(), |d: &ast::LetBinding| d.ud());
        for vs in grouped.values() {
            for v in vs {
                match v {
                    ast::LetBinding::Sig(l, _) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                        break
                    }
                    ast::LetBinding::Name(l, _, _) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                        break
                    }
                    ast::LetBinding::Pattern(_, _) => {}
                }
            }
        }
        for v in grouped.get(&None).iter().copied().flatten() {
            match v {
                ast::LetBinding::Sig(..)
                | ast::LetBinding::Name(..) => {
                    unreachable!()
                }
                ast::LetBinding::Pattern(b, e) => {
                    self.expr(e);
                    self.binder(b);
                }
            }
        }
        for (k, vs) in grouped.iter() {
            if k.is_none() { continue }
            for v in vs {
                match v {
                    ast::LetBinding::Sig(name, t) => {
                        self.resolve(Term, None, name.0);
                        self.typ(t);
                    }
                    ast::LetBinding::Name(name, bs, e) => {
                        self.resolve(Term, None, name.0);
                        let sf = self.push();
                        for b in bs.iter() {
                            self.binder(b);
                        }
                        self.guarded_expr(e);
                        self.pop(sf);
                    }
                    ast::LetBinding::Pattern(_, _) => { }
                }
            }
        }
    }

    fn binder(&mut self, b: &ast::Binder) {
        match b {
            ast::Binder::Typed(b, t) => {
                // let sf = self.push();
                self.typ(t);
                self.binder(b);
                // TODO: This is incorrect - it should only pop the pushed types.
                // self.pop(sf);
            }
            ast::Binder::App(a, b) => {
                self.binder(a);
                self.binder(b);
            }
            ast::Binder::Op(a, o, b) => {
                self.binder(a);
                self.resolveq(Term, o.0, o.1 .0);
                self.binder(b);
            }
            ast::Binder::Wildcard(_) => (),
            ast::Binder::Var(name) => {
                self.def_local(Term, name.0 .0, name.0 .1);
            }
            ast::Binder::Named(name, b) => {
                self.def_local(Term, name.0 .0, name.0 .1);
                self.binder(b);
            }
            ast::Binder::Constructor(c) => {
                self.resolveq(Term, c.0, c.1 .0);
            }
            ast::Binder::Boolean(_) => (),
            ast::Binder::Char(_) => (),
            ast::Binder::Str(_) => (),
            ast::Binder::Number(_, _) => (),
            ast::Binder::Array(ts) => {
                for b in ts.iter() {
                    self.binder(b);
                }
            }
            ast::Binder::Record(bs) => {
                for b in bs.iter() {
                    match b {
                        ast::RecordLabelBinder::Pun(l) => {
                            self.def_local(Term, l.0 .0, l.0 .1);
                        }
                        ast::RecordLabelBinder::Field(_, b) => {
                            self.binder(b);
                        }
                    }
                }
            }
            ast::Binder::Paren(b) => {
                self.binder(b);
            }
        }
    }

    fn constraint(&mut self, ast::Constraint(c, ts): &ast::Constraint) {
        self.resolveq(Class, c.0, c.1 .0);
        for t in ts.iter() {
            self.typ(t);
        }
    }

    fn typ_define_vars(&mut self, t: &ast::Typ) {
        match t {
            ast::Typ::Wildcard(_)
            | ast::Typ::Constructor(_)
            | ast::Typ::Symbol(_)
            | ast::Typ::Str(_)
            | ast::Typ::Int(_, _)
            | ast::Typ::Hole(_) => (),

            ast::Typ::Var(v) => {
                if self.find_local(Type, v.0 .0).is_none() {
                    self.def_local(Type, v.0 .0, v.0 .1);
                } else {
                    self.resolve(Type, None, v.0);
                }
            }
            ast::Typ::Record(rs) | ast::Typ::Row(rs) => {
                let rs = &rs.0;
                for (_, t) in rs.0.iter() {
                    self.typ_define_vars(t);
                }
                if let Some(t) = &rs.1 {
                    self.typ_define_vars(t);
                }
            }
            ast::Typ::Forall(xs, t) => {
                let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                self.typ_define_vars(t);
                self.pop(sf);
            }
            ast::Typ::Kinded(t, _t) => {
                // Not doing Kinds for now
                self.typ_define_vars(t);
            }
            ast::Typ::Arr(a, b) => {
                self.typ_define_vars(a);
                self.typ_define_vars(b);
            }
            ast::Typ::Op(a, _, b) => {
                self.typ_define_vars(a);
                self.typ_define_vars(b);
            }
            ast::Typ::Constrained(_, t) => {
                self.typ_define_vars(t);
            }
            ast::Typ::App(a, b) => {
                self.typ_define_vars(a);
                self.typ_define_vars(b);
            }
            ast::Typ::Paren(a) => {
                self.typ_define_vars(a);
            }
        }
    }

    fn typ(&mut self, t: &ast::Typ) {
        match t {
            ast::Typ::Wildcard(_) => (),
            ast::Typ::Var(v) => {
                self.resolve(Type, None, v.0);
            }
            ast::Typ::Constructor(v) => {
                self.resolveq(Type, v.0, v.1 .0);
            }
            ast::Typ::Symbol(v) => {
                self.resolveq(Type, v.0, v.1 .0);
            }
            ast::Typ::Str(_) => (),
            ast::Typ::Int(_, _) => (),
            ast::Typ::Hole(_) => (),
            ast::Typ::Record(rs) | ast::Typ::Row(rs) => {
                let rs = &rs.0;
                for (_, t) in rs.0.iter() {
                    self.typ(t);
                }
                if let Some(t) = &rs.1 {
                    self.typ(t);
                }
            }
            ast::Typ::Forall(xs, t) => {
                // Has to be handled by caller
                // let sf = self.push();
                for x in xs.iter() {
                    self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                self.typ(t);
                // self.pop(sf);
            }
            ast::Typ::Kinded(t, _t) => {
                // Not doing Kinds for now
                self.typ(t);
            }
            ast::Typ::Arr(a, b) => {
                self.typ(a);
                self.typ(b);
            }
            ast::Typ::Op(a, o, b) => {
                self.typ(a);
                self.resolveq(Type, o.0, o.1 .0);
                self.typ(b);
            }
            ast::Typ::Constrained(c, t) => {
                self.constraint(c);
                self.typ(t);
            }
            ast::Typ::App(a, b) => {
                self.typ(a);
                self.typ(b);
            }
            ast::Typ::Paren(a) => {
                self.typ(a);
            }
        }
    }
}

// Build a map of all source positions that have a name connected with them. We can then use
// that mapping to update the global mapping.
pub fn resolve_names(n: &mut N, prim: ast::Ud, m: &ast::Module) -> Option<ast::Ud> {
    // You still get syntax errors - but without a module-header we can't verify the names in
    // the module. This is annoying and could possibly be fixed.
    if let Some(h) = m.0.as_ref() {
        let name = h.0 .0 .0;
        n.me = name;
        n.exports
            .push(Export::Just(Name(Module, name, name, Visibility::Public)));
        n.def_global(Module, h.0 .0, true);
        // Inject the Prim import
        n.import(&ast::ImportDecl {
            from: ast::MName(ast::S(prim, ast::Span::Zero)),
            hiding: Vec::new(),
            names: Vec::new(),
            to: None,
        });
        for i in h.2.iter() {
            n.import(i);
        }
        let grouped = group_by(m.1.iter(), |d: &ast::Decl| d.ud());
        for ds in grouped.values() {
            let mut is_redecl = false;
            for d in ds.iter() {
                n.decl_first(d, is_redecl);
                is_redecl = true;
            }
        }
        for ds in grouped.values() {
            let sf = n.push();
            for d in ds.iter() {
                n.decl_body(d);
            }
            n.pop(sf);
        }

        if let Some(exports) = &h.1 {
            for ex in exports.iter() {
                n.export(ex);
            }
        } else {
            n.export_self();
        }

        Some(h.0 .0 .0)
    } else {
        None
    }
}

fn group_by<'s, K, V>(
    iter: std::slice::Iter<'s, V>,
    key: impl Fn(&'s V) -> K,
) -> BTreeMap<K, Vec<&'s V>>
where
    K: Ord,
{
    let mut out = BTreeMap::new();
    for i in iter {
        out.entry(key(i)).or_insert(Vec::new()).push(i);
    }
    out
}
