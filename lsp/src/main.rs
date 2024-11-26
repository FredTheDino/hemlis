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

    modules: DashMap<ast::Ud, ast::Module>,
    resolved: DashMap<ast::Ud, BTreeMap<Pos, Name>>,
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
        let cur = lut.lower_bound(Bound::Included(&pos));
        let lo_name = cur.peek_prev()?;
        let hi_name = cur.peek_next()?;
        // There's a sneaky bug here - consider:
        //
        // type _ = B -> * A
        // type B = Int
        //
        // if you place the cursor at * - and "GOTO-DEFINITION" you goto B - even though it's
        // not a valid symbol. This can be solved by either adding a random value to the name -
        // which might be more sane than the line/col and 0 I have now. But this change has
        // annoying implications. If there's a random value, we have to do an expensive lookup
        // to get the name which makes this code less "paralelizable". Given that `A` is an
        // undefined symbol - I think this is a niche enough case to warrent ignoring for now.
        if !(*lo_name.0 <= pos || pos <= *hi_name.0 && *lo_name.1 != *hi_name.1) {
            return None;
        }
        Some(*hi_name.1)
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
    pub struct N {
        pub me: ast::Ud,

        // NOTE: Maybe this should be a `&mut DashMap<Ud, BTreeMap<Name, BTreeSet<ast::Span>>>` instead
        pub usages: BTreeMap<Name, BTreeSet<ast::Span>>,

        pub errors: Vec<NRerrors>,

        pub resolved: BTreeMap<Pos, Name>,
        pub exports: Vec<Export>,

        constructors: BTreeMap<Name, BTreeSet<Name>>,
        module_imports: BTreeMap<ast::Ud, Vec<(ast::Ud, ast::Span)>>,

        imports: BTreeMap<(Scope, ast::Ud), Name>,

        pub defines: BTreeMap<Name, Pos>,
        locals: Vec<(Scope, ast::Ud, Name)>,
    }

    impl N {
        pub fn new() -> Self {
            Self {
                // :(((((( This is not correct!!! This state is set inside a member function.
                me: ast::Ud(0),
                usages: BTreeMap::new(),

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
            self.resolved.insert(s.lo(), name);
            self.resolved.insert(s.hi(), name);
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
                error!("RESOLVED: {:?} {:?}", s.lo(), *name);
                self.resolved.insert(s.lo(), *name);
                self.resolved.insert(s.hi(), *name);
                self.usages
                    .entry(*name)
                    .or_insert(BTreeSet::new())
                    .insert(s);
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
            }

            if self
                .defines
                .get(&Name(ss, m, n, Visibility::Public))
                .is_some()
            {
                return Some(Name(ss, m, n, Visibility::Public));
            }

            if m == self.me {
                if let Some(name) = self.imports.get(&(ss, n)) {
                    return Some(*name);
                }
            } else {
                // let name = (ss, m, n, Export::Public);
                // if self.global_defines.get(name).is_some() {
                //     return Some(name);
                // }
            }

            None
        }
    }

    // Build a map of all source positions that have a name connected with them. We can then use
    // that mapping to update the global mapping.
    pub fn resolve_names(n: &mut N, m: &ast::Module) -> Option<ast::Ud> {
        // You still get syntax errors - but without a module-header we can't verify the names in
        // the module. This is annoying and could possibly be fixed.
        if let Some(h) = m.0.as_ref() {
            let name = def_mname(n, &h.0);
            // NOTE[et]: I don't want this to be done here - but it's way easier to place it here
            n.me = name;
            for i in h.2.iter() {
                import(n, i);
            }
            let mut prev = None;
            for d in m.1.iter() {
                prev = decl_first(n, d, prev);
            }
            for d in m.1.iter() {
                let sf = n.push();
                decl_body(n, d);
                n.pop(sf);
            }

            for ex in h.1.iter() {
                export(n, ex);
            }

            Some(h.0 .0 .0)
        } else {
            None
        }
    }

    #[derive(Debug)]
    enum Export {
        DataSome(Name, Vec<Name>),
        DataAll(Name, Vec<Name>),
        Just(Name),
        Module(Name),
    }

    fn export(n: &mut N, ex: &ast::Export) {
        use Export::*;
        let export = match ex {
            ast::Export::Value(v) => Just(Name(Term, n.me, v.0 .0, Visibility::Public)),
            ast::Export::Symbol(v) => Just(Name(Term, n.me, v.0 .0, Visibility::Public)),
            ast::Export::Typ(v) => Just(Name(Type, n.me, v.0 .0, Visibility::Public)),
            ast::Export::TypSymbol(v) => Just(Name(Type, n.me, v.0 .0, Visibility::Public)),
            ast::Export::TypDat(v, ds) => {
                let x = Name(Type, n.me, v.0 .0, Visibility::Public);
                let ms = match n.constructors.get(&x) {
                    None => {
                        n.errors.push(NRerrors::NoConstructors(x, v.0.1));
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
                                    n.errors.push(NRerrors::NotAConstructor(x, *m));
                                    None
                                }
                            })
                            .collect(),
                    ),
                }
            }
            ast::Export::Class(v) => Just(Name(Type, n.me, v.0 .0, Visibility::Public)),
            ast::Export::Module(v) => Just(Name(Scope::Module, n.me, v.0 .0, Visibility::Public)),
        };
        n.exports.push(export);
    }

    fn import( n: &mut N, i: &ast::ImportDecl) {
        // NOTE: Is the export a usage? IDK...
        match i {
            ast::ImportDecl::As(a, b) => {
                match n.module_imports.entry(a.0 .0) {
                    std::collections::btree_map::Entry::Vacant(v) => {
                        v.insert(vec![(b.0 .0, b.0 .1)]);
                    }
                    std::collections::btree_map::Entry::Occupied(mut v) => {
                        v.get_mut().push((b.0 .0, b.0 .1));
                    }
                }
                n.def_global(Module, b.0, false);
            }
            ast::ImportDecl::Multiple(a, imports) => {
                for i in imports {
                    match i {
                        ast::Import::Value(x) => n.def_import(Term, a.0 .0, x.0),
                        ast::Import::Symbol(x) => n.def_import(Term, a.0 .0, x.0),
                        ast::Import::Typ(x) => n.def_import(Type, a.0 .0, x.0),
                        ast::Import::TypDat(x, _) => {
                            // TODO: The data-members
                            n.def_import(Type, a.0 .0, x.0)
                        }
                        ast::Import::TypSymbol(x) => n.def_import(Type, a.0 .0, x.0),
                        ast::Import::Class(x) => n.def_import(Type, a.0 .0, x.0),
                    }
                }
            }
            ast::ImportDecl::Hiding(a, _) => {
                // TODO: Hiding imports are kinda tricky
                match n.module_imports.entry(n.me) {
                    std::collections::btree_map::Entry::Vacant(v) => {
                        v.insert(vec![(a.0 .0, a.0 .1)]);
                    }
                    std::collections::btree_map::Entry::Occupied(mut v) => {
                        v.get_mut().push((a.0 .0, a.0 .1));
                    }
                }
            }
            ast::ImportDecl::Bulk(a) => match n.module_imports.entry(n.me) {
                std::collections::btree_map::Entry::Vacant(v) => {
                    v.insert(vec![(a.0 .0, a.0 .1)]);
                }
                std::collections::btree_map::Entry::Occupied(mut v) => {
                    v.get_mut().push((a.0 .0, a.0 .1));
                }
            },
        }
    }

    fn def_mname(n: &mut N, h: &ast::MName) -> ast::Ud {
        n.def_global(Module, h.0, true);
        h.0 .0
    }

    // TODO: This is needs to be split into two passes - one for the initial declarations and
    // one for inner declarations - since there is no order here. One could also push these
    // references first and check them later - saying where the same declaration is used in e.g
    // error messages.
    fn decl_first(n: &mut N, d: &ast::Decl, prev: Option<ast::Ud>) -> Option<ast::Ud> {
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
                n.def_global(Type, d.0, prev == q);
                q
            }
            ast::Decl::Data(d, _, cs) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                let mut cons = BTreeSet::new();
                for c in cs {
                    n.def_global(Term, c.0.0, false);
                    cons.insert(Name(Term, n.me, c.0.0.0, Visibility::Public));
                }
                n.constructors.insert(Name(Type, n.me, d.0.0, Visibility::Public), cons);
                None
            }
            ast::Decl::TypeKind(d, _) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                q
            }
            ast::Decl::Type(d, xs, t) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                // Bug compatible with the Purs-compiler
                n.constructors.insert(Name(Type, n.me, d.0.0, Visibility::Public), BTreeSet::new());
                None
            }
            ast::Decl::NewTypeKind(d, _) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                q
            }
            ast::Decl::NewType(d, xs, c, t) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                n.def_global(Term, c.0, false);
                n.constructors.insert(Name(Type, n.me, d.0.0, Visibility::Public), [Name(Term, n.me, c.0.0, Visibility::Public)].into());
                None
            }
            ast::Decl::ClassKind(d, _) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                q
            }
            ast::Decl::Class(cs, d, xs, fd, mem) => {
                let q = Some(d.0 .0);
                n.def_global(Type, d.0, prev == q);
                q
            }
            ast::Decl::Foreign(d, _) => {
                n.def_global(Term, d.0, false);
                None
            }
            ast::Decl::ForeignData(d, _) => {
                n.def_global(Type, d.0, false);
                None
            }
            ast::Decl::Fixity(_, _, _, o) => {
                n.def_global(Term, o.0, false);
                None
            }
            ast::Decl::FixityTyp(_, _, _, o) => {
                n.def_global(Type, o.0, false);
                None
            }
            ast::Decl::Sig(d, _) => {
                n.def_global(Type, d.0, false);
                Some(d.0 .0)
            }
            ast::Decl::Def(d, _, _) => {
                let q = Some(d.0 .0);
                n.def_global(Term, d.0, prev == q);
                q
            }
            ast::Decl::Instance(_, _, _) => None,
            ast::Decl::Derive(_, _) => None,
            ast::Decl::Role(_, _) => None,
        }
    }

    fn decl_body(n: &mut N, d: &ast::Decl) {
        // I skipped references in Kinds for now - not because it's hard but because I
        // want a demo ASAP and it has little value.
        match d {
            ast::Decl::DataKind(_, _) => (),
            ast::Decl::Data(d, xs, cs) => {
                let sf = n.push();
                for x in xs.iter() {
                    n.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                for c in cs {
                    for t in c.1.iter() {
                        typ(n, t);
                    }
                }
                n.pop(sf);
            }
            ast::Decl::TypeKind(_, _) => (),
            ast::Decl::Type(d, xs, t) => {
                let sf = n.push();
                for x in xs.iter() {
                    n.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                typ(n, t);
                n.pop(sf);
            }
            ast::Decl::NewTypeKind(_, _) => (),
            ast::Decl::NewType(d, xs, c, t) => {
                let sf = n.push();
                for x in xs.iter() {
                    n.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                typ(n, t);
                n.pop(sf);
            }
            ast::Decl::ClassKind(_, _) => (),
            ast::Decl::Class(cs, d, xs, fd, mem) => {
                let sf = n.push();
                for x in xs.iter() {
                    n.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                for c in cs {
                    constraint(n, c);
                }
                n.pop(sf);
            }
            ast::Decl::Instance(_, head, bindings) => {
                let u = inst_head(n, head);
                for b in bindings.iter() {
                    inst_binding(n, b, u);
                }
            }
            ast::Decl::Derive(_, head) => {
                let _u = inst_head(n, head);
            }
            ast::Decl::Foreign(d, t) => {
                typ(n, t);
            }
            ast::Decl::ForeignData(_, _) => {}
            ast::Decl::Role(d, _) => {
                n.resolve(Term, n.me, d.0);
            }
            ast::Decl::Fixity(_, _, e, _) => {
                expr(n, e);
            }
            ast::Decl::FixityTyp(_, _, t, _) => {
                typ(n, t);
            }
            ast::Decl::Sig(_, t) => {
                typ(n, t);
            }
            ast::Decl::Def(_, bs, e) => {
                for b in bs.iter() {
                    binder(n, b);
                }
                guarded_expr(n, e);
            }
        }
    }

    fn inst_head(n: &mut N, ast::InstHead(cs, d, ts): &ast::InstHead) -> ast::Ud {
        for c in cs.iter() {
            constraint(n, c);
        }
        n.resolveq(Type, d.0, d.1 .0);
        for t in ts.iter() {
            typ(n, t);
        }
        match d.0 {
            Some(u) => u.0 .0,
            None => n.me,
        }
    }

    fn inst_binding(n: &mut N, b: &ast::InstBinding, u: ast::Ud) {
        match b {
            ast::InstBinding::Sig(l, t) => {
                n.resolve(Type, u, l.0);
                typ(n, t);
            }
            ast::InstBinding::Def(l, binders, e) => {
                n.resolve(Type, u, l.0);
                let sf = n.push();
                for b in binders.iter() {
                    binder(n, b);
                }
                guarded_expr(n, e);
                n.pop(sf);
            }
        }
    }

    fn guarded_expr(n: &mut N, e: &ast::GuardedExpr) {
        match e {
            ast::GuardedExpr::Unconditional(e) => expr(n, e),
            ast::GuardedExpr::Guarded(es) => {
                for (gs, e) in es.iter() {
                    let sf = n.push();
                    for g in gs {
                        match g {
                            ast::Guard::Expr(e) => expr(n, e),
                            ast::Guard::Binder(b, e) => {
                                // NOTE[et]: We define things from the expression, so the flow of
                                // execution is different here.
                                expr(n, e);
                                binder(n, b);
                            }
                        }
                    }
                    expr(n, e);
                    n.pop(sf);
                }
            }
        }
    }

    fn expr(n: &mut N, e: &ast::Expr) {
        match e {
            ast::Expr::Typed(e, t) => {
                expr(n, e);
                typ(n, t);
            }
            ast::Expr::Op(a, o, b) => {
                expr(n, a);
                n.resolveq(Term, o.0, o.1 .0);
                expr(n, b);
            }
            ast::Expr::Infix(a, o, b) => {
                expr(n, a);
                expr(n, o);
                expr(n, b);
            }
            ast::Expr::Negate(e) => {
                expr(n, e);
            }
            ast::Expr::App(a, b) => {
                expr(n, a);
                expr(n, b);
            }
            ast::Expr::Vta(e, t) => {
                expr(n, e);
                typ(n, t);
            }
            ast::Expr::IfThenElse(_, a, tru, fal) => {
                expr(n, a);
                expr(n, tru);
                expr(n, fal);
            }
            ast::Expr::Do(_qual, stmts) => {
                // TODO: Add in the qualified to jump to the module/import
                let sf = n.push();
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(Some(b), e) => {
                            // NOTE[et]: We define things from the expression, so the flow of
                            // execution is different here.
                            expr(n, e);
                            binder(n, b);
                        }
                        ast::DoStmt::Stmt(None, e) => {
                            expr(n, e);
                        }
                        ast::DoStmt::Let(ls) => {
                            let_binders(n, ls);
                        }
                    }
                }
                n.pop(sf);
            }
            ast::Expr::Ado(_qual, stmts, ret) => {
                let sf = n.push();
                // NOTE[et]: We define things from the expression, so the flow of
                // execution is different here.
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(_, e) => {
                            expr(n, e);
                        }
                        ast::DoStmt::Stmt(_, e) => {
                            expr(n, e);
                        }
                        ast::DoStmt::Let(ls) => {}
                    }
                }
                for s in stmts.iter() {
                    match s {
                        ast::DoStmt::Stmt(Some(b), e) => {
                            binder(n, b);
                        }
                        ast::DoStmt::Stmt(None, e) => {}
                        ast::DoStmt::Let(ls) => {
                            let_binders(n, ls);
                        }
                    }
                }
                expr(n, ret);
                n.pop(sf);
            }
            ast::Expr::Lambda(_, bs, e) => {
                let sf = n.push();
                for b in bs.iter() {
                    binder(n, b);
                }
                expr(n, e);
                n.pop(sf);
            }
            ast::Expr::Where(_, e, ls) | ast::Expr::Let(_, ls, e) => {
                let sf = n.push();
                let_binders(n, ls);
                expr(n, e);
                n.pop(sf);
            }
            ast::Expr::Case(_, es, cs) => {
                for e in es.iter() {
                    expr(n, e);
                }
                for ast::CaseBranch(bs, e) in cs.iter() {
                    let sf = n.push();
                    for b in bs.iter() {
                        binder(n, b);
                    }
                    guarded_expr(n, e);
                    n.pop(sf);
                }
            }
            ast::Expr::Array(_, es, _) => {
                for e in es.iter() {
                    expr(n, e);
                }
            }
            ast::Expr::Record(_, rs, _) => {
                for r in rs {
                    match r {
                        ast::RecordLabelExpr::Pun(l) => {
                            n.resolve(Term, n.me, l.0);
                        }
                        ast::RecordLabelExpr::Field(_, e) => {
                            expr(n, e);
                        }
                    }
                }
            }
            ast::Expr::Update(e, rs) => {
                expr(n, e);
                for r in rs.iter() {
                    record_update(n, r);
                }
            }
            ast::Expr::Access(e, _) => {
                expr(n, e);
            }
            ast::Expr::Section(_) => (),
            ast::Expr::Hole(_) => (),
            ast::Expr::Ident(v) => {
                n.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Constructor(v) => {
                n.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Symbol(v) => {
                n.resolveq(Term, v.0, v.1 .0);
            }
            ast::Expr::Boolean(_) => (),
            ast::Expr::Char(_) => (),
            ast::Expr::Str(_) => (),
            ast::Expr::Number(_) => (),
            ast::Expr::HexInt(_) => (),
            ast::Expr::Paren(e) => {
                expr(n, e);
            }
        }
    }

    fn record_update(n: &mut N, r: &ast::RecordUpdate) {
        match r {
            ast::RecordUpdate::Leaf(_, e) => {
                expr(n, e);
            }
            ast::RecordUpdate::Branch(_, rs) => {
                for r in rs.iter() {
                    record_update(n, r);
                }
            }
        }
    }

    fn let_binders(n: &mut N, ls: &Vec<ast::LetBinding>) {
        for l in ls {
            match l {
                ast::LetBinding::Sig(l, t) => {
                    n.def_local(Term, l.0 .0, l.0 .1);
                }
                ast::LetBinding::Name(l, bs, e) => {
                    n.def_local(Term, l.0 .0, l.0 .1);
                }
                ast::LetBinding::Pattern(b, e) => {
                    binder(n, b);
                }
            }
        }
        for l in ls {
            match l {
                ast::LetBinding::Sig(l, t) => {
                    typ(n, t);
                }
                ast::LetBinding::Name(l, bs, e) => {
                    let sf = n.push();
                    for b in bs.iter() {
                        binder(n, b);
                    }
                    guarded_expr(n, e);
                    n.pop(sf);
                }
                ast::LetBinding::Pattern(b, e) => {
                    expr(n, e);
                    binder(n, b);
                }
            }
        }
    }

    fn binder(n: &mut N, b: &ast::Binder) {
        match b {
            ast::Binder::Typed(b, t) => {
                binder(n, b);
                typ(n, t);
            }
            ast::Binder::App(a, b) => {
                binder(n, a);
                binder(n, b);
            }
            ast::Binder::Op(a, o, b) => {
                binder(n, a);
                n.resolveq(Term, o.0, o.1 .0);
                binder(n, b);
            }
            ast::Binder::Wildcard(_) => (),
            ast::Binder::Var(name) => {
                n.def_local(Term, name.0 .0, name.0 .1);
            }
            ast::Binder::Named(name, b) => {
                n.def_local(Term, name.0 .0, name.0 .1);
                binder(n, b);
            }
            ast::Binder::Constructor(c) => {
                n.resolveq(Term, c.0, c.1 .0);
            }
            ast::Binder::Boolean(_) => (),
            ast::Binder::Char(_) => (),
            ast::Binder::Str(_) => (),
            ast::Binder::Number(_, _) => (),
            ast::Binder::Array(ts) => {
                for b in ts.iter() {
                    binder(n, b);
                }
            }
            ast::Binder::Record(bs) => {
                for b in bs.iter() {
                    match b {
                        ast::RecordLabelBinder::Pun(l) => {
                            n.resolve(Term, n.me, l.0);
                        }
                        ast::RecordLabelBinder::Field(_, b) => {
                            binder(n, b);
                        }
                    }
                }
            }
            ast::Binder::Paren(b) => {
                binder(n, b);
            }
        }
    }

    fn constraint(n: &mut N, ast::Constraint(c, ts): &ast::Constraint) {
        n.resolveq(Type, c.0, c.1 .0);
        for t in ts.iter() {
            typ(n, t);
        }
    }

    fn typ(n: &mut N, t: &ast::Typ) {
        match t {
            ast::Typ::Wildcard(_) => (),
            ast::Typ::Var(v) => {
                n.resolve(Type, n.me, v.0);
            }
            ast::Typ::Constructor(v) => {
                n.resolveq(Type, v.0, v.1 .0);
            }
            ast::Typ::Symbol(v) => {
                n.resolveq(Type, v.0, v.1 .0);
            }
            ast::Typ::Str(_) => (),
            ast::Typ::Int(_) => (),
            ast::Typ::Hole(_) => (),
            ast::Typ::Record(rs) | ast::Typ::Row(rs) => {
                let rs = &rs.0;
                for (_, t) in rs.0.iter() {
                    typ(n, t);
                }
                if let Some(t) = &rs.1 {
                    typ(n, t);
                }
            }
            ast::Typ::Forall(xs, t) => {
                let sf = n.push();
                for x in xs.iter() {
                    n.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                }
                typ(n, t);
                n.pop(sf);
            }
            ast::Typ::Kinded(t, _t) => {
                // Not doing Kinds for now
                typ(n, t);
            }
            ast::Typ::Arr(a, b) => {
                typ(n, a);
                typ(n, b);
            }
            ast::Typ::Op(a, o, b) => {
                typ(n, a);
                n.resolveq(Type, o.0, o.1 .0);
                typ(n, b);
            }
            ast::Typ::Constrained(c, t) => {
                constraint(n, c);
                typ(n, t);
            }
            ast::Typ::App(a, b) => {
                typ(n, a);
                typ(n, b);
            }
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
        // TODO: Strip out old usages
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
        let mut n = name_resolution::N::new();
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
                    name_resolution::NRerrors::NotAConstructor(d, m) => 
                        Diagnostic::new_simple(
                            Range::new(pos_from_tup(m.0.1.lo()), pos_from_tup(m.0.1.hi())),
                            format!(
                                "{} does not have a constructors {}",
                                self.names
                                    .get(&d.2)
                                    .map(|x| x.clone())
                                    .unwrap_or_else(|| "?".into()),
                                self.names
                                    .get(&m.0.0)
                                    .map(|x| x.clone())
                                    .unwrap_or_else(|| "?".into())
                            ),
                        ),
                    name_resolution::NRerrors::NoConstructors(m, s) =>
                        Diagnostic::new_simple(
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
