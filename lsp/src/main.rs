#![feature(btree_cursors)]

use std::collections::{BTreeMap, BTreeSet};
use std::ops::Bound;

use dashmap::DashMap;
use log::{debug, error};
use purring_lib::*;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    names: DashMap<ast::Ud, String>,

    url_to_ud: DashMap<String, ast::Ud>,
    ud_to_url: DashMap<ast::Ud, Url>,
    fi_to_ud: DashMap<ast::Fi, ast::Ud>,
    url_to_fi: DashMap<String, ast::Fi>,

    exports: DashMap<ast::Ud, Vec<Export>>,
    modules: DashMap<ast::Ud, ast::Module>,
    resolved: DashMap<ast::Ud, BTreeMap<(Pos, Pos), Name>>,
    defines: DashMap<Name, Pos>,
    usages: DashMap<ast::Ud, BTreeMap<Name, BTreeSet<ast::Span>>>,
}

impl Backend {
    fn resolve_name(&self, url: &String, pos: Position) -> Option<Name> {
        let m = *self.url_to_ud.get(url)?;
        // error!("M: {:?}", m);
        // NOTE: might have an off-by-one
        let pos = (pos.line as usize, (pos.character + 1) as usize);
        let lut = self.resolved.get(&m)?;
        error!("POS: {:?}", pos);
        for pos in lut.iter() {
            error!("LUT: {:?}", pos);
        }
        let cur = lut.lower_bound(Bound::Included(&(pos, pos)));
        let ((lo, hi), name) = cur.peek_prev()?;
        if !(*lo <= pos && pos <= *hi) {
            return None;
        }
        Some(*name)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                completion_provider: None,
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: None,
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        debug!("initialized!");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        debug!("file saved!");
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed!");
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let name = self.resolve_name(
                &params
                    .text_document_position_params
                    .text_document
                    .uri
                    .to_string(),
                params.text_document_position_params.position,
            )?;
            let def_at = self.defines.get(&name)?;
            let uri = self.ud_to_url.get(&name.1)?.clone();
            Some(GotoDefinitionResponse::Scalar(Location {
                uri,
                range: Range {
                    start: pos_from_tup(*def_at),
                    end: pos_from_tup(*def_at),
                },
            }))
        }();
        Ok(definition)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let name = self.resolve_name(
                &params.text_document_position.text_document.uri.to_string(),
                params.text_document_position.position,
            )?;
            Some(
                self.usages
                    .get(&name.1)?
                    .get(&name)?
                    .iter()
                    .filter_map(|s: &ast::Span| {
                        let m = self.fi_to_ud.get(&s.fi()?)?;
                        let url = self.ud_to_url.get(&m)?;
                        let lo = pos_from_tup(s.lo());
                        let hi = pos_from_tup(s.hi());

                        let range = Range::new(lo, hi);

                        Some(Location::new(url.clone(), range))
                    })
                    .collect::<Vec<_>>(),
            )
        }();
        Ok(reference_list)
    }

    /*
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let ast = self.ast_map.get(&uri.to_string())?;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let completions = completion(&ast, offset);
            let mut ret = Vec::with_capacity(completions.len());
            for (_, item) in completions {
                match item {
                    nrs_language_server::completion::ImCompleteCompletionItem::Variable(var) => {
                        ret.push(CompletionItem {
                            label: var.clone(),
                            insert_text: Some(var.clone()),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: Some(var),
                            ..Default::default()
                        });
                    }
                    nrs_language_server::completion::ImCompleteCompletionItem::Function(
                        name,
                        args,
                    ) => {
                        ret.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(name.clone()),
                            insert_text: Some(format!(
                                "{}({})",
                                name,
                                args.iter()
                                    .enumerate()
                                    .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
                                    .collect::<Vec<_>>()
                                    .join(",")
                            )),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        });
                    }
                }
            }
            Some(ret)
        }();
        Ok(completions.map(CompletionResponse::Array))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let semantic = self.semantic_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;
            let position = params.text_document_position.position;
            let offset = position_to_offset(position, &rope)?;
            let reference_list = get_references(&semantic, offset, offset + 1, true)?;

            let new_name = params.new_name;
            (!reference_list.is_empty()).then_some(()).map(|_| {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|range| {
                        let start_position = offset_to_position(range.start, &rope)?;
                        let end_position = offset_to_position(range.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                WorkspaceEdit::new(map)
            })
        }();
        Ok(workspace_edit)
    }
    */

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        debug!("configuration changed!");
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        debug!("workspace folders changed!");
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        debug!("watched files have changed!");
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        debug!("command executed!");

        // Rename? Extract functions? Add import? Add export?
        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}

pub type Pos = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Visibility {
    Private((usize, usize)),
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Scope, ast::Ud, ast::Ud, Visibility);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scope {
    Kind,
    Type,
    Term,
    Module,
}

#[derive(Debug)]
enum Export {
    DataSome(Name, Vec<Name>),
    DataAll(Name, Vec<Name>),
    Just(Name),
    Module(Vec<Name>),
}
impl Export {
    fn contains(&self, name: Name) -> bool {
        match self {
            Export::DataSome(n, xs) => *n == name || xs.iter().any(|x| *x == name),
            Export::DataAll(n, xs) => *n == name || xs.iter().any(|x| *x == name),
            Export::Just(n) => *n == name,
            Export::Module(xs) => xs.iter().any(|x| *x == name),
        }
    }
}

#[allow(unused)]
mod name_resolution {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    use super::Scope::*;

    use purring_lib::{ast, lexer, parser};

    // Deliberately not `Copy` so you always use it right
    #[derive(Debug)]
    pub struct Sf(usize);

    #[derive(Debug)]
    pub enum NRerrors {
        Unknown(ast::Span),
        MultipleImports(Vec<(ast::Span, Name)>, ast::Span),
        MultipleDefinitions(Name, (usize, usize), (usize, usize)),
        NotAConstructor(Name, ast::ProperName),
        NoConstructors(Name, ast::Span),
    }

    #[derive(Debug)]
    pub struct N<'s> {
        pub me: ast::Ud,

        global_exports: &'s DashMap<ast::Ud, Vec<Export>>,
        pub global_usages: Vec<(Name, ast::Span)>,

        // NOTE: Maybe this should be a `&mut DashMap<Ud, BTreeMap<Name, BTreeSet<ast::Span>>>` instead
        pub usages: BTreeMap<Name, BTreeSet<ast::Span>>,

        pub errors: Vec<NRerrors>,

        pub resolved: BTreeMap<(Pos, Pos), Name>,
        pub exports: Vec<Export>,

        constructors: BTreeMap<Name, BTreeSet<Name>>,
        module_imports: BTreeMap<ast::Ud, Vec<(ast::Ud, ast::Span)>>,

        imports: BTreeMap<(Scope, ast::Ud), Name>,

        pub defines: BTreeMap<Name, Pos>,
        locals: Vec<(Scope, ast::Ud, Name)>,
    }

    impl<'s> N<'s> {
        pub fn new(global_exports: &'s DashMap<ast::Ud, Vec<Export>>) -> Self {
            Self {
                // :(((((( This is not correct!!! This state is set inside a member function.
                me: ast::Ud(0),
                usages: BTreeMap::new(),
                global_exports,
                global_usages: Vec::new(),

                errors: Vec::new(),
                resolved: BTreeMap::new(),
                exports: Vec::new(),
                constructors: BTreeMap::new(),
                module_imports: BTreeMap::new(),
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
                    v.insert(s.lo());
                }
                std::collections::btree_map::Entry::Occupied(v) => {
                    if ignore_error {
                        ();
                    } else {
                        self.errors
                            .push(NRerrors::MultipleDefinitions(*v.key(), *v.get(), s.lo()));
                    }
                }
            }
            self.resolved.insert((s.lo(), s.hi()), name);
            self.usages.entry(name).or_insert(BTreeSet::new()).insert(s);
        }

        fn def_global(&mut self, scope: Scope, s: ast::S<ast::Ud>, is_redecl: bool) {
            let name = Name(scope, self.me, s.0, Visibility::Public);
            self.def(s.1, name, is_redecl)
        }

        fn def_local(&mut self, scope: Scope, u: ast::Ud, s: ast::Span) {
            let name = Name(scope, self.me, u, Visibility::Private(s.lo()));
            self.locals.push((scope, u, name));

            self.def(s, name, false)
        }

        fn def_import(&mut self, scope: Scope, m: ast::Ud, s: ast::S<ast::Ud>) {
            let name = Name(scope, m, s.0, Visibility::Private(s.1.lo()));

            self.def(s.1, name, false)
        }

        fn resolve(&mut self, scope: Scope, m: ast::Ud, n: ast::S<ast::Ud>) {
            let s = n.1;
            let n = n.0;
            error!("resolve: {:?}", (scope, m, n));
            let mut matches = Vec::new();
            if m == self.me {
                if let Some(name) = self.resolve_inner(scope, m, n) {
                    matches.push((s, name));
                }
            }
            if matches.is_empty() {
                for (nn, ss) in self.module_imports.get(&m).iter().flat_map(|x| x.iter()) {
                    if let Some(name) = self.resolve_inner(scope, *nn, n) {
                        matches.push((*ss, name));
                    }
                }
            }

            let unique_matches = matches
                .iter()
                .map(|(_, name)| *name)
                .collect::<BTreeSet<_>>();
            for name in unique_matches.iter() {
                self.resolved.insert((s.lo(), s.hi()), *name);
                if name.1 == self.me {
                    self.usages
                        .entry(*name)
                        .or_insert(BTreeSet::new())
                        .insert(s);
                } else {
                    self.global_usages.push((*name, s));
                }
            }

            match unique_matches.len() {
                0 => self.errors.push(NRerrors::Unknown(s)),
                1 => (),
                _ => self.errors.push(NRerrors::MultipleImports(matches, s)),
            }
        }

        fn resolveq(&mut self, scope: Scope, m: Option<ast::Qual>, n: ast::S<ast::Ud>) {
            match m {
                Some(m) => {
                    self.resolve(Module, self.me, m.0);
                    self.resolve(scope, m.0 .0, n);
                }
                None => {
                    self.resolve(scope, self.me, n);
                }
            }
        }

        // For `A.B.C.foo` does `A.B.C` resolve to the module - or does it resolve to `foo`?
        fn resolve_inner(&self, ss: Scope, m: ast::Ud, n: ast::Ud) -> Option<Name> {
            if m == self.me {
                if let Some((_, _, name)) =
                    self.locals.iter().rfind(|(s, u, _)| *u == n && *s == ss)
                {
                    return Some(*name);
                }

                if self
                    .defines
                    .get(&Name(ss, m, n, Visibility::Public))
                    .is_some()
                {
                    return Some(Name(ss, m, n, Visibility::Public));
                }

                if let Some(name) = self.imports.get(&(ss, n)) {
                    return Some(*name);
                }
            } else {
                let name = Name(ss, m, n, Visibility::Public);
                if self
                    .global_exports
                    .get(&m)
                    .map(|x| x.iter().any(|ex| ex.contains(name)))
                    .unwrap_or_else(|| false)
                {
                    return Some(name);
                }
                // TODO: Say what is exported
            }

            None
        }

        fn export(&mut self, ex: &ast::Export) {
            use Export::*;
            let export = match ex {
                ast::Export::Value(v) => Just(Name(Term, self.me, v.0 .0, Visibility::Public)),
                ast::Export::Symbol(v) => Just(Name(Term, self.me, v.0 .0, Visibility::Public)),
                ast::Export::Typ(v) => Just(Name(Type, self.me, v.0 .0, Visibility::Public)),
                ast::Export::TypSymbol(v) => Just(Name(Type, self.me, v.0 .0, Visibility::Public)),
                ast::Export::TypDat(v, ds) => {
                    let x = Name(Type, self.me, v.0 .0, Visibility::Public);
                    let ms = match self.constructors.get(&x) {
                        None => {
                            self.errors.push(NRerrors::NoConstructors(x, v.0 .1));
                            return;
                        }
                        Some(ms) => ms,
                    };
                    match ds {
                        ast::DataMember::All => DataAll(x, ms.iter().cloned().collect()),
                        ast::DataMember::Some(ns) => DataSome(
                            x,
                            ns.iter()
                                .filter_map(|m| match ms.iter().find(|a| a.2 == m.0 .0) {
                                    Some(a) => Some(*a),
                                    None => {
                                        self.errors.push(NRerrors::NotAConstructor(x, *m));
                                        None
                                    }
                                })
                                .collect(),
                        ),
                    }
                }
                ast::Export::Class(v) => Just(Name(Type, self.me, v.0 .0, Visibility::Public)),
                ast::Export::Module(v) => {
                    Just(Name(Scope::Module, self.me, v.0 .0, Visibility::Public))
                }
            };
            self.exports.push(export);
        }

        fn import_resolve(&mut self, h: ast::MName) {
            // We can't use the normal resolve - because?
            let name = Name(Module, h.0.0, h.0.0, Visibility::Public);
            self.global_usages.push((name, h.0.1));
            self.resolved.insert((h.0.1.lo(), h.0.1.hi()), name);
        }

        fn import(&mut self, i: &ast::ImportDecl) {
            // NOTE: Is the export a usage? IDK...
            match i {
                ast::ImportDecl::As(a, b) => {
                    self.import_resolve(*a);
                    match self.module_imports.entry(a.0 .0) {
                        std::collections::btree_map::Entry::Vacant(v) => {
                            v.insert(vec![(b.0 .0, b.0 .1)]);
                        }
                        std::collections::btree_map::Entry::Occupied(mut v) => {
                            v.get_mut().push((b.0 .0, b.0 .1));
                        }
                    }
                    self.def_global(Module, b.0, false);
                }
                ast::ImportDecl::Multiple(a, imports) => {
                    self.import_resolve(*a);
                    for i in imports {
                        match i {
                            ast::Import::Value(x) => self.def_import(Term, a.0 .0, x.0),
                            ast::Import::Symbol(x) => self.def_import(Term, a.0 .0, x.0),
                            ast::Import::Typ(x) => self.def_import(Type, a.0 .0, x.0),
                            ast::Import::TypDat(x, _) => {
                                // TODO: The data-members
                                self.def_import(Type, a.0 .0, x.0)
                            }
                            ast::Import::TypSymbol(x) => self.def_import(Type, a.0 .0, x.0),
                            ast::Import::Class(x) => self.def_import(Type, a.0 .0, x.0),
                        }
                    }
                }
                ast::ImportDecl::Hiding(a, _) => {
                    self.import_resolve(*a);
                    // TODO: Hiding imports are kinda tricky
                    match self.module_imports.entry(self.me) {
                        std::collections::btree_map::Entry::Vacant(v) => {
                            v.insert(vec![(a.0 .0, a.0 .1)]);
                        }
                        std::collections::btree_map::Entry::Occupied(mut v) => {
                            v.get_mut().push((a.0 .0, a.0 .1));
                        }
                    }
                }
                ast::ImportDecl::Bulk(a) => {
                    self.import_resolve(*a);
                    match self.module_imports.entry(self.me) {
                        std::collections::btree_map::Entry::Vacant(v) => {
                            v.insert(vec![(a.0 .0, a.0 .1)]);
                        }
                        std::collections::btree_map::Entry::Occupied(mut v) => {
                            v.get_mut().push((a.0 .0, a.0 .1));
                        }
                    }
                }
            }
        }

        // TODO: This is needs to be split into two passes - one for the initial declarations and
        // one for inner declarations - since there is no order here. One could also push these
        // references first and check them later - saying where the same declaration is used in e.g
        // error messages.
        fn decl_first(&mut self, d: &ast::Decl, prev: Option<ast::Ud>) -> Option<ast::Ud> {
            // I skipped references in Kinds for now - not because it's hard but because I
            // want a demo ASAP and it has little value.
            //
            // NOTE: There's a sneaky bug here, where multiple kind definitions don't cause an error. I
            // don't think this will be that annoying - but it requires more through analysis to
            // resolve. There's also the case with term-definitions where there can be guards - this
            // requires more sophisticated checking. (Or just returning None if it's a catch-all?)
            match d {
                ast::Decl::DataKind(d, _) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    q
                }
                ast::Decl::Data(d, _, cs) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    let mut cons = BTreeSet::new();
                    for c in cs {
                        self.def_global(Term, c.0 .0, false);
                        cons.insert(Name(Term, self.me, c.0 .0 .0, Visibility::Public));
                    }
                    self.constructors
                        .insert(Name(Type, self.me, d.0 .0, Visibility::Public), cons);
                    None
                }
                ast::Decl::TypeKind(d, _) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    q
                }
                ast::Decl::Type(d, xs, t) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    // Bug compatible with the Purs-compiler
                    self.constructors.insert(
                        Name(Type, self.me, d.0 .0, Visibility::Public),
                        BTreeSet::new(),
                    );
                    None
                }
                ast::Decl::NewTypeKind(d, _) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    q
                }
                ast::Decl::NewType(d, xs, c, t) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    self.def_global(Term, c.0, false);
                    self.constructors.insert(
                        Name(Type, self.me, d.0 .0, Visibility::Public),
                        [Name(Term, self.me, c.0 .0, Visibility::Public)].into(),
                    );
                    None
                }
                ast::Decl::ClassKind(d, _) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    q
                }
                ast::Decl::Class(cs, d, xs, fd, mem) => {
                    let q = Some(d.0 .0);
                    self.def_global(Type, d.0, prev == q);
                    q
                }
                ast::Decl::Foreign(d, _) => {
                    self.def_global(Term, d.0, false);
                    None
                }
                ast::Decl::ForeignData(d, _) => {
                    self.def_global(Type, d.0, false);
                    None
                }
                ast::Decl::Fixity(_, _, _, o) => {
                    self.def_global(Term, o.0, false);
                    None
                }
                ast::Decl::FixityTyp(_, _, _, o) => {
                    self.def_global(Type, o.0, false);
                    None
                }
                ast::Decl::Sig(d, _) => {
                    self.def_global(Type, d.0, false);
                    Some(d.0 .0)
                }
                ast::Decl::Def(d, _, _) => {
                    let q = Some(d.0 .0);
                    self.def_global(Term, d.0, prev == q);
                    q
                }
                ast::Decl::Instance(_, _, _) => None,
                ast::Decl::Derive(_, _) => None,
                ast::Decl::Role(_, _) => None,
            }
        }

        fn decl_body(&mut self, d: &ast::Decl) {
            // I skipped references in Kinds for now - not because it's hard but because I
            // want a demo ASAP and it has little value.
            match d {
                ast::Decl::DataKind(_, _) => (),
                ast::Decl::Data(d, xs, cs) => {
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
                ast::Decl::Type(d, xs, t) => {
                    let sf = self.push();
                    for x in xs.iter() {
                        self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                    }
                    self.typ(t);
                    self.pop(sf);
                }
                ast::Decl::NewTypeKind(_, _) => (),
                ast::Decl::NewType(d, xs, c, t) => {
                    let sf = self.push();
                    for x in xs.iter() {
                        self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                    }
                    self.typ(t);
                    self.pop(sf);
                }
                ast::Decl::ClassKind(_, _) => (),
                ast::Decl::Class(cs, d, xs, fd, mem) => {
                    let sf = self.push();
                    for x in xs.iter() {
                        self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                    }
                    for c in cs {
                        self.constraint(c);
                    }
                    self.pop(sf);
                }
                ast::Decl::Instance(_, head, bindings) => {
                    let u = self.inst_head(head);
                    for b in bindings.iter() {
                        self.inst_binding(b, u);
                    }
                }
                ast::Decl::Derive(_, head) => {
                    let _u = self.inst_head(head);
                }
                ast::Decl::Foreign(d, t) => {
                    self.typ(t);
                }
                ast::Decl::ForeignData(_, _) => {}
                ast::Decl::Role(d, _) => {
                    self.resolve(Term, self.me, d.0);
                }
                ast::Decl::Fixity(_, _, e, _) => {
                    self.expr(e);
                }
                ast::Decl::FixityTyp(_, _, t, _) => {
                    self.typ(t);
                }
                ast::Decl::Sig(_, t) => {
                    self.typ(t);
                }
                ast::Decl::Def(_, bs, e) => {
                    for b in bs.iter() {
                        self.binder(b);
                    }
                    self.guarded_expr(e);
                }
            }
        }

        fn inst_head(&mut self, ast::InstHead(cs, d, ts): &ast::InstHead) -> ast::Ud {
            for c in cs.iter() {
                self.constraint(c);
            }
            self.resolveq(Type, d.0, d.1 .0);
            for t in ts.iter() {
                self.typ(t);
            }
            match d.0 {
                Some(u) => u.0 .0,
                None => self.me,
            }
        }

        fn inst_binding(&mut self, b: &ast::InstBinding, u: ast::Ud) {
            match b {
                ast::InstBinding::Sig(l, t) => {
                    self.resolve(Type, u, l.0);
                    self.typ(t);
                }
                ast::InstBinding::Def(l, binders, e) => {
                    self.resolve(Type, u, l.0);
                    let sf = self.push();
                    for b in binders.iter() {
                        self.binder(b);
                    }
                    self.guarded_expr(e);
                    self.pop(sf);
                }
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
                    self.expr(e);
                    self.typ(t);
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
                    self.typ(t);
                }
                ast::Expr::IfThenElse(_, a, tru, fal) => {
                    self.expr(a);
                    self.expr(tru);
                    self.expr(fal);
                }
                ast::Expr::Do(_qual, stmts) => {
                    // TODO: Add in the qualified to jump to the module/import
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
                ast::Expr::Ado(_qual, stmts, ret) => {
                    let sf = self.push();
                    // NOTE[et]: We define things from the expression, so the flow of
                    // execution is different here.
                    for s in stmts.iter() {
                        match s {
                            ast::DoStmt::Stmt(_, e) => {
                                self.expr(e);
                            }
                            ast::DoStmt::Stmt(_, e) => {
                                self.expr(e);
                            }
                            ast::DoStmt::Let(ls) => {}
                        }
                    }
                    for s in stmts.iter() {
                        match s {
                            ast::DoStmt::Stmt(Some(b), e) => {
                                self.binder(b);
                            }
                            ast::DoStmt::Stmt(None, e) => {}
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
                                self.resolve(Term, self.me, l.0);
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

        fn let_binders(&mut self, ls: &Vec<ast::LetBinding>) {
            for l in ls {
                match l {
                    ast::LetBinding::Sig(l, t) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                    }
                    ast::LetBinding::Name(l, bs, e) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                    }
                    ast::LetBinding::Pattern(b, e) => {
                        self.binder(b);
                    }
                }
            }
            for l in ls {
                match l {
                    ast::LetBinding::Sig(l, t) => {
                        self.typ(t);
                    }
                    ast::LetBinding::Name(l, bs, e) => {
                        let sf = self.push();
                        for b in bs.iter() {
                            self.binder(b);
                        }
                        self.guarded_expr(e);
                        self.pop(sf);
                    }
                    ast::LetBinding::Pattern(b, e) => {
                        self.expr(e);
                        self.binder(b);
                    }
                }
            }
        }

        fn binder(&mut self, b: &ast::Binder) {
            match b {
                ast::Binder::Typed(b, t) => {
                    self.binder(b);
                    self.typ(t);
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
                                self.resolve(Term, self.me, l.0);
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
            self.resolveq(Type, c.0, c.1 .0);
            for t in ts.iter() {
                self.typ(t);
            }
        }

        fn typ(&mut self, t: &ast::Typ) {
            match t {
                ast::Typ::Wildcard(_) => (),
                ast::Typ::Var(v) => {
                    self.resolve(Type, self.me, v.0);
                }
                ast::Typ::Constructor(v) => {
                    self.resolveq(Type, v.0, v.1 .0);
                }
                ast::Typ::Symbol(v) => {
                    self.resolveq(Type, v.0, v.1 .0);
                }
                ast::Typ::Str(_) => (),
                ast::Typ::Int(_) => (),
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
                    let sf = self.push();
                    for x in xs.iter() {
                        self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                    }
                    self.typ(t);
                    self.pop(sf);
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
            }
        }
    }

    // Build a map of all source positions that have a name connected with them. We can then use
    // that mapping to update the global mapping.
    pub fn resolve_names(n: &mut N, m: &ast::Module) -> Option<ast::Ud> {
        // You still get syntax errors - but without a module-header we can't verify the names in
        // the module. This is annoying and could possibly be fixed.
        if let Some(h) = m.0.as_ref() {
            // NOTE[et]: This is very finicky code. :(
            // NOTE[et]: I don't want this to be done here - but it's way easier to place it here
            // than in some requirement for `N`.
            let name = h.0 .0 .0;
            n.me = name;
            n.exports.push(Export::Just(Name(Module, name, name, Visibility::Public)));
            n.def_global(Module, h.0 .0, true);
            for i in h.2.iter() {
                n.import(i);
            }
            let mut prev = None;
            for d in m.1.iter() {
                prev = n.decl_first(d, prev);
            }
            for d in m.1.iter() {
                let sf = n.push();
                n.decl_body(d);
                n.pop(sf);
            }

            for ex in h.1.iter() {
                n.export(ex);
            }

            Some(h.0 .0 .0)
        } else {
            None
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

#[allow(unused)]
enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem<'a> {
    uri: Url,
    text: &'a str,
    version: Option<i32>,
}

impl Backend {
    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        let fi = match self.url_to_fi.entry(params.uri.to_string()) {
            dashmap::Entry::Occupied(v) => *v.get(),
            dashmap::Entry::Vacant(v) => {
                let fi = ast::Fi(sungod::Ra::ggen::<usize>());
                v.insert(fi);
                fi
            }
        };
        let l = lexer::lex(params.text, fi);
        let mut p = parser::P::new(&l, &self.names);
        let m = parser::module(&mut p);
        let mut n = name_resolution::N::new(&self.exports);
        if let Some(m) = m {
            name_resolution::resolve_names(&mut n, &m);
            let me = n.me;
            self.fi_to_ud.insert(fi, me);
            self.url_to_ud.insert(params.uri.to_string(), me);
            self.ud_to_url.insert(me, params.uri.clone());
            self.modules.insert(me, m);
            self.resolved.insert(me, n.resolved);
            self.usages.insert(me, n.usages);
            for (name, pos) in n.defines.iter() {
                self.defines.insert(*name, *pos);
            }
            for (name, pos) in n.global_usages.into_iter() {
                if let Some(mut e) = self.usages.get_mut(&name.1) {
                    if let Some(e) = e.get_mut(&name) {
                        e.insert(pos);
                    }
                }
            }
            self.exports.insert(me, n.exports);
        };

        let diagnostics: std::vec::Vec<tower_lsp::lsp_types::Diagnostic> = [
            p.errors
                .into_iter()
                .map(|err| {
                    let message = match err {
                        parser::Serror::Info(_, s) => format!("Info: {}", s),
                        parser::Serror::Unexpected(_, t, s) => format!("Unexpected {:?}: {}", t, s),
                        parser::Serror::NotSimpleTypeVarBinding(_) => {
                            format!("Not a simple type-var binding")
                        }
                        parser::Serror::NotAConstraint(_) => format!("Not a constraint"),
                        parser::Serror::NotAtEOF(_, _) => format!("Not at end of file"),
                        parser::Serror::FailedToParseDecl(_, _, _, _) => {
                            format!("Failed to parse this declaration")
                        }
                    };
                    let span = err.span();

                    Diagnostic::new_simple(
                        Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                        message,
                    )
                })
                .collect::<Vec<_>>(),
            n.errors
                .into_iter()
                .map(|err| match err {
                    name_resolution::NRerrors::Unknown(span) => Diagnostic::new_simple(
                        Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                        "Failed to resolve this name".into(),
                    ),
                    name_resolution::NRerrors::MultipleImports(_, span) => Diagnostic::new_simple(
                        Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                        "This name is imported from two different modules".into(),
                    ),
                    name_resolution::NRerrors::MultipleDefinitions(
                        Name(scope, _m, i, _),
                        _first,
                        second,
                    ) => Diagnostic::new_simple(
                        Range::new(pos_from_tup(second), pos_from_tup(second)),
                        format!(
                            "{:?} {:?} is defined multiple times",
                            scope,
                            self.names
                                .get(&i)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into())
                        ),
                    ),
                    name_resolution::NRerrors::NotAConstructor(d, m) => Diagnostic::new_simple(
                        Range::new(pos_from_tup(m.0 .1.lo()), pos_from_tup(m.0 .1.hi())),
                        format!(
                            "{} does not have a constructors {}",
                            self.names
                                .get(&d.2)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            self.names
                                .get(&m.0 .0)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into())
                        ),
                    ),
                    name_resolution::NRerrors::NoConstructors(m, s) => Diagnostic::new_simple(
                        Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                        format!(
                            "{} does not have constructors",
                            self.names
                                .get(&m.2)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                        ),
                    ),
                })
                .collect::<Vec<_>>(),
        ]
        .concat();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
            .await;
    }
}

fn pos_from_tup((line, col): (usize, usize)) -> Position {
    Position::new(line as u32, col as u32)
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        names: DashMap::new(),
        exports: DashMap::new(),
        url_to_ud: DashMap::new(),
        ud_to_url: DashMap::new(),
        fi_to_ud: DashMap::new(),
        url_to_fi: DashMap::new(),
        modules: DashMap::new(),
        resolved: DashMap::new(),
        defines: DashMap::new(),
        usages: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
