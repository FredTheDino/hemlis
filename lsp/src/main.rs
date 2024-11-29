#![feature(btree_cursors)]

use rayon::prelude::*;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Bound;

use dashmap::DashMap;
use log::{debug, error};
use hemlis_lib::*;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,

    builtins: BTreeSet<(Scope, ast::Ud)>,
    names: DashMap<ast::Ud, String>,

    fi_to_url: DashMap<ast::Fi, Url>,
    fi_to_ud: DashMap<ast::Fi, ast::Ud>,
    url_to_fi: DashMap<Url, ast::Fi>,
    fi_to_version: DashMap<ast::Fi, Option<i32>>,
    ud_to_fi: DashMap<ast::Ud, ast::Fi>,

    importers: DashMap<ast::Ud, BTreeSet<ast::Ud>>,
    imports: DashMap<ast::Ud, BTreeSet<ast::Ud>>,

    previouse_global_usages: DashMap<ast::Fi, BTreeSet<(Name, ast::Span)>>,
    previouse_defines: DashMap<ast::Fi, BTreeSet<(Name, ast::Span)>>,

    exports: DashMap<ast::Ud, Vec<Export>>,
    modules: DashMap<ast::Ud, ast::Module>,
    resolved: DashMap<ast::Ud, BTreeMap<(Pos, Pos), Name>>,
    defines: DashMap<Name, ast::Span>,
    usages: DashMap<ast::Ud, BTreeMap<Name, BTreeSet<ast::Span>>>,

    syntax_errors: DashMap<ast::Fi, Vec<tower_lsp::lsp_types::Diagnostic>>,
    name_resolution_errors: DashMap<ast::Fi, Vec<tower_lsp::lsp_types::Diagnostic>>,
}

impl Backend {
    fn resolve_name(&self, url: &Url, pos: Position) -> Option<Name> {
        let m = self.fi_to_ud.get(&*self.url_to_fi.get(url)?)?;
        let pos = (pos.line as usize, (pos.character + 1) as usize);
        let lut = self.resolved.get(&m)?;
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
        error!("Started loading workspace!");
        self.load_workspace().await;
        error!("Finished loading workspace!");
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

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
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
                    .uri,
                params.text_document_position_params.position,
            )?;
            let def_at = self.defines.get(&name)?;
            let uri = self.fi_to_url.get(&def_at.fi()?)?.clone();
            Some(GotoDefinitionResponse::Scalar(Location {
                uri,
                range: Range {
                    start: pos_from_tup(def_at.lo()),
                    end: pos_from_tup(def_at.hi()),
                },
            }))
        }();
        Ok(definition)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let name = self.resolve_name(
                &params.text_document_position.text_document.uri,
                params.text_document_position.position,
            )?;
            Some(
                self.usages
                    .get(&name.1)?
                    .get(&name)?
                    .iter()
                    .filter_map(|s: &ast::Span| {
                        let url = self.fi_to_url.get(&s.fi()?)?;
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
    Namespace,
}

#[derive(Debug, Clone, Hash)]
enum Export {
    ConstructorsSome(Name, Vec<Name>),
    ConstructorsAll(Name, Vec<Name>),
    Just(Name),
    Module(Vec<Name>),
}
impl Export {
    fn contains(&self, name: Name) -> bool {
        match self {
            Export::ConstructorsSome(n, xs) | Export::ConstructorsAll(n, xs) => {
                *n == name || xs.iter().any(|x| *x == name)
            }
            Export::Just(n) => *n == name,
            Export::Module(xs) => xs.iter().any(|x| *x == name),
        }
    }

    fn contains_(&self, s: Scope, u: ast::Ud) -> bool {
        match self {
            Export::ConstructorsAll(n, xs) | Export::ConstructorsSome(n, xs) => {
                name_is(*n, s, u) || xs.iter().any(|x| name_is(*x, s, u))
            }
            Export::Just(n) => name_is(*n, s, u),
            Export::Module(xs) => xs.iter().any(|x| name_is(*x, s, u)),
        }
    }

    fn to_names(&self) -> Vec<Name> {
        match self {
            Export::ConstructorsAll(n, xs) | Export::ConstructorsSome(n, xs) => {
                [vec![*n], xs.to_vec()].concat()
            }
            Export::Just(n) => vec![*n],
            Export::Module(xs) => xs.clone(),
        }
    }
}

fn name_is(Name(ss, _, uu, _): Name, s: Scope, u: ast::Ud) -> bool {
    ss == s && uu == u
}

mod name_resolution {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    use super::Scope::*;

    use hemlis_lib::ast;

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
        ConstructorsDoesntExistOrIsntExported(ast::S<ast::Ud>, ast::S<ast::Ud>),
        NoConstructorOfThatName(ast::S<ast::Ud>, ast::Ud, ast::Ud, ast::Span),
        NotExportedOrDoesNotExist(ast::S<ast::Ud>, Scope, ast::Ud, ast::Span),
        CannotImportSelf(ast::Span),
    }

    impl NRerrors {
        pub fn turn_into_diagnostic(
            self,
            names: &DashMap<ast::Ud, String>,
        ) -> tower_lsp::lsp_types::Diagnostic {
            match self {
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
                        names
                            .get(&i)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into())
                    ),
                ),
                name_resolution::NRerrors::NotAConstructor(d, m) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(m.0 .1.lo()), pos_from_tup(m.0 .1.hi())),
                    format!(
                        "{} does not have a constructors {}",
                        names
                            .get(&d.2)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                        names
                            .get(&m.0 .0)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into())
                    ),
                ),
                name_resolution::NRerrors::NoConstructors(m, s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    format!(
                        "{} does not have constructors",
                        names
                            .get(&m.2)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                    ),
                ),
                name_resolution::NRerrors::ConstructorsDoesntExistOrIsntExported(
                    m,
                    ast::S(u, s),
                ) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    format!(
                        "{}.{} does not exist or has no constructors exported",
                        names
                            .get(&m.0)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                        names
                            .get(&u)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                    ),
                ),

                name_resolution::NRerrors::NoConstructorOfThatName(m, a, b, s) => {
                    Diagnostic::new_simple(
                        Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                        format!(
                            "{} is not an exported constructor for the type {}.{}",
                            names
                                .get(&b)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            names
                                .get(&m.0)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            names
                                .get(&a)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                        ),
                    )
                }
                name_resolution::NRerrors::NotExportedOrDoesNotExist(m, scope, ud, s) => {
                    Diagnostic::new_simple(
                        Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                        format!(
                            "{:?} {}.{} is not exported or does not exist",
                            scope,
                            names
                                .get(&m.0)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            names
                                .get(&ud)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                        ),
                    )
                }
                name_resolution::NRerrors::CannotImportSelf(s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    "A module cannot import itself".to_string(),
                ),
            }
        }
    }

    #[derive(Debug)]
    pub struct N<'s> {
        pub me: ast::Ud,


        builtins: &'s BTreeSet<(Scope, ast::Ud)>,

        global_exports: &'s DashMap<ast::Ud, Vec<Export>>,
        pub global_usages: BTreeSet<(Name, ast::Span)>,

        // NOTE: Maybe this should be a `&mut DashMap<Ud, BTreeMap<Name, BTreeSet<ast::Span>>>` instead
        pub usages: BTreeMap<Name, BTreeSet<ast::Span>>,

        pub errors: Vec<NRerrors>,

        pub resolved: BTreeMap<(Pos, Pos), Name>,
        pub exports: Vec<Export>,

        constructors: BTreeMap<Name, BTreeSet<Name>>,
        pub module_imports: BTreeMap<Option<ast::Ud>, Vec<(ast::Ud, ast::Span)>>,

        imports: BTreeMap<(Scope, ast::Ud), Name>,

        pub defines: BTreeMap<Name, ast::Span>,
        locals: Vec<(Scope, ast::Ud, Name)>,
    }

    impl<'s> N<'s> {
        pub fn new(me: ast::Ud, builtins: &'s BTreeSet<(Scope, ast::Ud)>, global_exports: &'s DashMap<ast::Ud, Vec<Export>>) -> Self {
            Self {
                me,
                usages: BTreeMap::new(),

                builtins,

                global_exports,
                global_usages: BTreeSet::new(),

                errors: Vec::new(),
                resolved: BTreeMap::new(),
                exports: Vec::new(),
                constructors: BTreeMap::new(),
                module_imports: [(None, vec![(me, ast::Span::Zero)])].into(),
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
                        self.errors
                            .push(NRerrors::MultipleDefinitions(*v.key(), v.get().lo(), s.lo()));
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
            self.locals.push((scope, u, name));

            self.def(s, name, false)
        }

        fn resolve(&mut self, scope: Scope, m: ast::Ud, n: ast::S<ast::Ud>) {
            let s = n.1;
            let n = n.0;
            let mut matches = Vec::new();
            if m == self.me {
                if let Some(name) = self.resolve_inner(scope, m, n) {
                    matches.push((s, name));
                }
            }
            if matches.is_empty() {
                for (nn, ss) in self.module_imports.get(&Some(m)).iter().flat_map(|x| x.iter()) {
                    if let Some(name) = self.resolve_inner(scope, *nn, n) {
                        matches.push((*ss, name));
                    }
                }
            }

            // TODO: This is technically incorrect, since this is only valid if it is not a
            // namespace at all.
            if matches.is_empty() && m == self.me && self.builtins.contains(&(scope, n)) {
                matches.push((s, Name(scope, ast::Ud(0), n, Visibility::Public)));
            }

            let unique_matches = matches
                .iter()
                .map(|x| x.1)
                .collect::<BTreeSet<_>>();
            let num_unique_matches = unique_matches.len();
            for name in unique_matches.into_iter() {
                self.resolved.insert((s.lo(), s.hi()), name);
                if name.1 == self.me {
                    self.usages.entry(name).or_default().insert(s);
                } else {
                    self.global_usages.insert((name, s));
                }
            }

            match num_unique_matches {
                0 => self.errors.push(NRerrors::Unknown(s)),
                1 => (),
                _ => self.errors.push(NRerrors::MultipleImports(matches, s)),
            }
        }

        fn resolveq(&mut self, scope: Scope, m: Option<ast::Qual>, n: ast::S<ast::Ud>) {
            let m = m.map(|x| {
                    self.resolve(Namespace, self.me, x.0);
                x.0.0
            });
            self.resolve(scope, m.unwrap_or(self.me), n);
        }

        // For `A.B.C.foo` does `A.B.C` resolve to the module - or does it resolve to `foo`?
        fn resolve_inner(&self, ss: Scope, m: ast::Ud, n: ast::Ud) -> Option<Name> {
            if m != self.me {
                let name = Name(ss, m, n, Visibility::Public);
                if self
                    .global_exports
                    .get(&m)
                    .map(|x| x.iter().any(|ex| ex.contains(name)))
                    .unwrap_or_else(|| false)
                {
                    return Some(name);
                }
            } else {
                if let Some((_, _, name)) =
                    self.locals.iter().rfind(|(s, u, _)| *u == n && *s == ss)
                {
                    return Some(*name);
                }

                if self
                    .defines
                    .contains_key(&Name(ss, self.me, n, Visibility::Public))
                {
                    return Some(Name(ss, self.me, n, Visibility::Public));
                }

                if let Some(name) = self.imports.get(&(ss, n)) {
                    return Some(*name);
                }
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
                        ast::DataMember::All => ConstructorsAll(x, ms.iter().copied().collect()),
                        ast::DataMember::Some(ns) => ConstructorsSome(
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
                    if let Some(vs) = self.module_imports.get(&Some(v.0 .0)) {
                        Module(
                            vs.iter()
                                .map(|(a, _)| Name(Scope::Module, *a, *a, Visibility::Public))
                                .collect(),
                        )
                    } else if let Some((a, _)) = self.module_imports.values().flatten().find(|(x, _)| *x == v.0.0) {
                        Module(vec![Name(Scope::Module, *a, *a, Visibility::Public)])
                    } else {
                        self.errors.push(NRerrors::Unknown(v.0 .1));
                        return;
                    }
                }
            };
            self.exports.push(export);
        }

        fn import_resolve(&mut self, h: ast::MName) {
            // We can't use the normal resolve - because?
            let name = Name(Module, h.0 .0, h.0 .0, Visibility::Public);
            self.global_usages.insert((name, h.0 .1));
            self.resolved.insert((h.0 .1.lo(), h.0 .1.hi()), name);
        }

        fn register_import_as(&mut self, a: ast::MName, b: Option<ast::MName>) {
            self.module_imports.entry(b.map(|x| x.0 .0)).or_default().push((a.0.0, a.0.1));
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
            // NOTE: Is the export a usage? IDK...

            self.import_resolve(*from);
            self.register_import_as(*from, *to);
            if let Some(b) = to {
                self.def_global(Namespace, b.0, false);
            }
            if !self.global_exports.contains_key(&from.0 .0) {
                return;
            }
            if from.0 .0 == self.me {
                self.errors.push(NRerrors::CannotImportSelf(from.0 .1));
            }
            let exports: Vec<Export> = self.global_exports.get(&from.0 .0).unwrap().value().clone();

            let mut ii = Vec::new();
            // TODO: I'm gonna need a test-suite for this
            // NOTE[et]: I've choosen to ignore hiding imports and re-exporting that module, because it really complicates things...
            for i in names {
                match i {
                    ast::Import::Value(x) => ii.push((Term, x.0)),
                    ast::Import::Symbol(x) => ii.push((Term, x.0)),
                    ast::Import::Typ(x) => ii.push((Type, x.0)),
                    ast::Import::TypDat(x, cs) => {
                        ii.push((Type, x.0));
                        let es = match exports.iter().find(|e| e.contains_(Type, x.0 .0)) {
                            Some(
                                Export::ConstructorsSome(_, es) | Export::ConstructorsAll(_, es),
                            ) => es.iter().map(|x| x.2).collect::<BTreeSet<_>>(),

                            _ => {
                                self.errors
                                    .push(NRerrors::ConstructorsDoesntExistOrIsntExported(
                                        from.0, x.0,
                                    ));
                                BTreeSet::new()
                            }
                        };
                        match cs {
                            ast::DataMember::All => {
                                for c in es.iter() {
                                    ii.push((Term, ast::S(*c, x.0 .1)))
                                }
                            }
                            ast::DataMember::Some(xs) => {
                                for xx in xs {
                                    if es.contains(&xx.0 .0) {
                                        ii.push((Term, xx.0));
                                    } else {
                                        self.errors.push(NRerrors::NoConstructorOfThatName(
                                            from.0, x.0 .0, xx.0 .0, xx.0 .1,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    ast::Import::TypSymbol(x) => ii.push((Type, x.0)),
                    ast::Import::Class(x) => ii.push((Type, x.0)),
                }
            }
            let mut exports: Vec<_> = exports.into_iter().map(|x| x.to_names()).collect();
            let valid: BTreeMap<_, _> = exports
                .iter()
                .flatten()
                .copied()
                .map(|name @ Name(s, _, u, _)| ((s, u), name))
                .collect();
            if !hiding.is_empty() {
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
                            ast::Import::Class(x) => (Type, x.0),
                        };
                        if let Some(n) = valid.get(&(s, u.0)) {
                            // Opinionatedly not adding imports to usages
                            self.resolved.insert((u.1.lo(), u.1.hi()), *n);
                        } else {
                            self.errors
                                .push(NRerrors::NotExportedOrDoesNotExist(from.0, s, u.0, u.1))
                        }
                        // NOTE[et]: This is bug-compatible with purs
                        if matches!(x, ast::Import::TypDat(_, ast::DataMember::Some(_))) {
                            return None;
                        }
                        Some((s, u.0))
                    })
                    .collect::<BTreeSet<_>>();
                exports.retain(|x| !x.iter().any(|Name(s, _, u, _)| hiding.contains(&(*s, *u))));
            }

            if ii.is_empty() {
                for (k, v) in valid.into_iter() {
                    self.imports.insert(k, v);
                }
            } else {
                for (k, v) in ii
                    .iter()
                    .filter_map(|(s, u)| {
                        let span = u.1;
                        let u = u.0;
                        let out = valid.get(&(*s, u));
                        if let Some(n) = out {
                            // Opinionatedly not adding imports to usages
                            self.resolved.insert((span.lo(), span.hi()), *n);
                        } else {
                            self.errors
                                .push(NRerrors::NotExportedOrDoesNotExist(from.0, *s, u, span));
                        }
                        Some(((*s, u), *out?))
                    })
                    .collect::<Vec<_>>()
                {
                    self.imports.insert(k, v);
                }
            }
        }

        // NOTE: This is needs to be split into two passes - one for the initial declarations and
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
                    // TODO: Type var bindings
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
                ast::Decl::Type(d, _, _) => {
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
                ast::Decl::NewType(d, _, c, _) => {
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
                ast::Decl::Class(cs, d, vars, deps, mem) => {
                    let q = Some(d.0 .0);

                    self.def_global(Type, d.0, prev == q);
                    for ast::ClassMember(name, _) in mem.iter() {
                        self.def_global(Term, name.0, false);
                    }
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
                ast::Decl::Class(cs, _, xs, _, mem) => {
                    // TODO: Fundeps should also be resolved here
                    let sf = self.push();
                    for x in xs.iter() {
                        self.def_local(Type, x.0 .0 .0, x.0 .0 .1);
                    }
                    for c in cs.iter().flatten() {
                        self.constraint(c);
                    }
                    self.pop(sf);
                    for ast::ClassMember(_, typ) in mem.iter() {
                        self.typ(typ);
                    }
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
                ast::Decl::Foreign(_, t) => {
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
            for c in cs.iter().flatten() {
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
                    self.resolve(Term, u, l.0);
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
                ast::Expr::Do(qual, stmts) => {
                    if let Some(qual) = qual {
                        self.resolve(Namespace, self.me, qual.0);
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
                        self.resolve(Namespace, self.me, qual.0);
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
                    ast::LetBinding::Sig(l, _) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                    }
                    ast::LetBinding::Name(l, _, _) => {
                        self.def_local(Term, l.0 .0, l.0 .1);
                    }
                    ast::LetBinding::Pattern(b, _) => {
                        self.binder(b);
                    }
                }
            }
            for l in ls {
                match l {
                    ast::LetBinding::Sig(_, t) => {
                        self.typ(t);
                    }
                    ast::LetBinding::Name(_, bs, e) => {
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
            n.exports
                .push(Export::Just(Name(Module, name, name, Visibility::Public)));
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

            if let Some(exports) = &h.1 {
                for ex in exports.iter() {
                    n.export(ex);
                }
            } else {
                let cs: BTreeSet<_> = n.constructors.values().flatten().collect();
                for name@Name(s, m, _, _) in n.defines.keys() {
                    if *m != n.me { continue; }
                    if !matches!(s, Scope::Term | Scope::Type) { continue; }
                    if cs.contains(name) { continue; }
                    let e = if let Some(co) = n.constructors.get(name) {
                        Export::ConstructorsAll(*name, co.iter().copied().collect::<Vec<_>>())
                    } else {
                        Export::Just(*name)
                    };
                    n.exports.push(e);

                }
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
    async fn load_workspace(&self) -> Option<()> {
        let folders = self.client.workspace_folders().await.ok()??;
        for folder in folders {
            use glob::glob;
            let deps = glob(&format!("{}/lib/**/*.purs", folder.uri.to_string().strip_prefix("file://")?)).ok()?.filter_map(|path| {
                error!("Found: {:?}", path);
                let path = path.as_ref().ok()?;
                let source = std::fs::read_to_string(path.clone()).ok()?;
                let url = Url::parse(&format!("file://{}", &path.clone().into_os_string().into_string().ok()?)).ok()?;
                let (m, fi) = self.parse(url, None, &source);
                let m = m?;
                let (me, imports) = {
                    let header = m.0.clone()?;
                    let me = header.0.0.0;
                    (me, header.2.iter().map(|x| x.from.0.0).collect::<BTreeSet<_>>())
                };
                self.modules.insert(me, m);
                self.fi_to_ud.insert(fi, me);
                self.ud_to_fi.insert(me, fi);
                error!("Parsed: {:?}", path);
                Some((me, fi, imports))
            }).collect::<Vec<_>>();

            let mut done = BTreeSet::new();
            loop {
                let todo: BTreeSet<_> = deps.par_iter().filter(|(m, _, deps)| (!done.contains(m)) && deps.is_subset(&done)).collect();
                if todo.is_empty() {
                    break;
                }
                todo.par_iter().for_each(|(m, fi, _)| { 
                    if let Some(m) = self.modules.get(m) {
                        self.resolve_module(&m, *fi);
                    }
                });
                done.append(&mut todo.into_iter().map(|(m, _, _)| *m).collect());
            }
        }
        Some(())
    }

    fn resolve_module(&self, m: &ast::Module, fi: ast::Fi) -> Option<(bool, ast::Ud)> {
        let me = m.0.as_ref()?.0.0.0;
        let mut n = name_resolution::N::new(me, &self.builtins, &self.exports);
        name_resolution::resolve_names(&mut n, m);
        let me = n.me;

        self.name_resolution_errors.insert(
            fi,
            n.errors
                .into_iter()
                .map(|x| x.turn_into_diagnostic(&self.names))
                .collect::<Vec<_>>(),
        );

        self.resolved.insert(me, n.resolved);

        let mut us = self.usages.entry(me).or_insert(BTreeMap::new());
        for (k, v) in us.iter_mut() {
            v.retain(|x| x.fi() != Some(fi));
            v.append(&mut n.usages.get(k).cloned().unwrap_or_default());
        }
        for (k, v) in n.usages.into_iter() {
            if us.contains_key(&k) {
                continue;
            }
            assert!(us.insert(k, v).is_none());
        }

        {
            let new = n.defines.into_iter().collect::<BTreeSet<_>>();
            let old = self
                .previouse_defines
                .insert(fi, new.clone())
                .unwrap_or_default();

            // Technically, we only need to remove the names which are not just deleted and not
            // moved. But this is easier to reason about.
            for (name, _) in old.difference(&new) {
                self.defines.remove(name);
            }
            for (name, pos) in new.difference(&old) {
                self.defines.insert(*name, *pos);
            }
        }

        {
            let new = n.global_usages;
            let old = self
                .previouse_global_usages
                .insert(fi, new.clone())
                .unwrap_or_default();
            for (name, pos) in old.difference(&new) {
                if let Some(mut e) = self.usages.get_mut(&name.1) {
                    if let Some(e) = e.get_mut(name) {
                        e.remove(pos);
                    }
                }
            }

            for (name, pos) in new.difference(&old) {
                let mut e = self.usages.entry(name.1).or_insert(BTreeMap::new());
                e.entry(*name).or_insert(BTreeSet::new()).insert(*pos);
            }
        }

        let exports_changed = {
            let new_hash = hash_exports(&n.exports);
            if let Some(old) = self.exports.insert(me, n.exports) {
                new_hash != hash_exports(&old)
            } else {
                true
            }
        };

        let new_imports: BTreeSet<_> = n.module_imports.values().flatten().map(|(u, _)| *u).collect();
        let old_imports = self
            .imports
            .insert(me, new_imports.clone())
            .unwrap_or_else(BTreeSet::new);
        for x in old_imports.difference(&new_imports) {
            let mut e = self.importers.entry(*x).or_insert(BTreeSet::new());
            e.remove(&me);
        }
        for x in new_imports.difference(&old_imports) {
            let mut e = self.importers.entry(*x).or_insert(BTreeSet::new());
            e.insert(me);
        }
        Some((exports_changed, n.me))
    }

    async fn resolve_cascading(&self, me: ast::Ud) {
        // TODO: This can be way way smarter, currently it only runs on changed exports.
        // Some exteions include: Lineage tracking - saying letting me know what parts actually
        // changed.
        let name = Name(Scope::Module, me, me, Visibility::Public);
        let mut checked = BTreeSet::new();
        let mut to_check: Vec<_> = self
            .importers
            .get(&me)
            .iter()
            .flat_map(|x| x.iter())
            .copied()
            .collect();
        while let Some(x) = to_check.pop() {
            if checked.contains(&x) {
                continue;
            }
            checked.insert(x);
            if let (Some(m), Some(fi)) = (self.modules.get(&x), self.ud_to_fi.get(&x)) {
                let _ = self.resolve_module(&m, *fi);
                self.show_errors(*fi).await;
                if let Some(ex) = self.exports.get(&x) {
                    if ex.iter().any(|x| x.contains(name)) {
                        // It's a re-export which means we need to check everything that imports it as well!
                        to_check.append(
                            &mut self
                                .importers
                                .get(&x)
                                .iter()
                                .flat_map(|x| x.iter())
                                .copied()
                                .collect::<Vec<_>>(),
                        );
                    }
                };
            }
        }
    }

    async fn show_errors(&self, fi: ast::Fi) {
        if let Some(url) = self.fi_to_url.get(&fi) {
            self.client
                .publish_diagnostics(
                    url.clone(),
                    [
                        if let Some(x) = self.syntax_errors.get(&fi) {
                            x.value().clone()
                        } else {
                            Vec::new()
                        },
                        if let Some(x) = self.name_resolution_errors.get(&fi) {
                            x.value().clone()
                        } else {
                            Vec::new()
                        },
                    ]
                    .concat(),
                    if let Some(v) = self.fi_to_version.get(&fi) {
                        *v
                    } else {
                        None
                    },
                )
                .await
        } else {
            panic!()
        }
    }

    fn parse(&self, uri: Url, version: Option<i32>, source: &'_ str) -> (Option<ast::Module>, ast::Fi) {
        // TODO: How to handle two files with the same module name?
        let fi = match self.url_to_fi.entry(uri.clone()) {
            dashmap::Entry::Occupied(v) => *v.get(),
            dashmap::Entry::Vacant(v) => {
                let fi = ast::Fi(sungod::Ra::ggen::<usize>());
                v.insert(fi);
                fi
            }
        };
        self.fi_to_version.insert(fi, version);
        self.fi_to_url.insert(fi, uri.clone());

        let l = lexer::lex(source, fi);
        let mut p = parser::P::new(&l, &self.names);
        let m = parser::module(&mut p);
        self.syntax_errors.insert(
            fi,
            p.errors
                .into_iter()
                .map(|err| {
                    let message = match err {
                        parser::Serror::Info(_, s) => format!("Info: {}", s),
                        parser::Serror::Unexpected(_, t, s) => format!("Unexpected {:?}: {}", t, s),
                        parser::Serror::NotSimpleTypeVarBinding(_) => {
                            "Not a simple type-var binding".to_string()
                        }
                        parser::Serror::NotAConstraint(_) => "Not a constraint".to_string(),
                        parser::Serror::NotAtEOF(_, _) => "Not at end of file".to_string(),
                        parser::Serror::FailedToParseDecl(_, _, _, _) => {
                            "Failed to parse this declaration".to_string()
                        }
                    };
                    let span = err.span();

                    Diagnostic::new_simple(
                        Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                        message,
                    )
                })
                .collect::<Vec<_>>(),
        );
        (m, fi)
    }

    async fn on_change(&self, params: TextDocumentItem<'_>) {
        let (m, fi) = self.parse(params.uri.clone(), params.version, params.text);
        error!("LOADED: {}", self.modules.len());
        //if self.modules.len() < 5 {
        //    self.load_workspace().await;
        //}

        // TODO: We could exit earlier if we have the same syntactical structure here
        if let Some(m) = m {
            if let Some((exports_changed, me)) = self.resolve_module(&m, fi) {
                self.modules.insert(me, m);
                self.fi_to_ud.insert(fi, me);
                self.ud_to_fi.insert(me, fi);

                self.show_errors(fi).await;
                if exports_changed {
                    self.resolve_cascading(me).await;
                }
            } else {
                self.show_errors(fi).await;
            }
        } else {
                self.show_errors(fi).await;
        }
    }
}

fn hash_exports(exports: &[Export]) -> u64 {
    let mut hasher = DefaultHasher::new();
    exports.hash(&mut hasher);
    hasher.finish()
}

fn pos_from_tup((line, col): (usize, usize)) -> Position {
    Position::new(line as u32, col as u32)
}

fn build_builtins() -> (BTreeSet<(Scope, ast::Ud)>, DashMap<ast::Ud, String>) {
    use Scope::*; 
    let names = DashMap::new();
    let h = |a: Scope, s: &'static str| -> (Scope, ast::Ud) {
        let mut hasher = DefaultHasher::new();
        s.hash(&mut hasher);
        let ud = ast::Ud(hasher.finish() as usize);
        names.insert(ud, s.into());
        (a, ud)
    };


    ([ h(Type, "Int"),
     h(Type, "Number"),
     h(Type, "Row"),
     h(Type, "Record"),
     h(Type, "Symbol"),
     h(Type, "Array"),
     h(Type, "Boolean"),
     h(Type, "String"),
     ].into(), names)
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    
    let (builtins, names) = build_builtins();

    let (service, socket) = LspService::build(|client|  {
        Backend {
        client,

        builtins,
        names,

        fi_to_url: DashMap::new(),
        fi_to_ud: DashMap::new(),
        ud_to_fi: DashMap::new(),
        url_to_fi: DashMap::new(),
        fi_to_version: DashMap::new(),

        importers: DashMap::new(),
        imports: DashMap::new(),

        previouse_defines: DashMap::new(),
        previouse_global_usages: DashMap::new(),

        exports: DashMap::new(),
        modules: DashMap::new(),
        resolved: DashMap::new(),
        defines: DashMap::new(),
        usages: DashMap::new(),

        syntax_errors: DashMap::new(),
        name_resolution_errors: DashMap::new(),
    }})
    .finish();


    Server::new(stdin, stdout, socket).serve(service).await;
}
