#![feature(btree_cursors)]
#![allow(clippy::type_complexity)]

use std::collections::{BTreeMap, BTreeSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Bound;

use dashmap::DashMap;
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

    prim: ast::Ud,

    names: DashMap<ast::Ud, String>,

    fi_to_url: DashMap<ast::Fi, Url>,
    fi_to_ud: DashMap<ast::Fi, ast::Ud>,
    url_to_fi: DashMap<Url, ast::Fi>,
    fi_to_version: DashMap<ast::Fi, Option<i32>>,
    ud_to_fi: DashMap<ast::Ud, ast::Fi>,

    importers: DashMap<ast::Ud, BTreeSet<ast::Ud>>,
    imports: DashMap<ast::Ud, BTreeSet<Export>>,

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
                    commands: vec!["load_workspace".to_string(), "random_command".to_string()],
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
        self.client
            .log_message(MessageType::ERROR, "Scanning...".to_string())
            .await;
        self.load_workspace().await;
        self.client
            .log_message(MessageType::ERROR, "Done scanning!".to_string())
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
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

    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let name = self.resolve_name(
                &params.text_document_position_params.text_document.uri,
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

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(
                MessageType::ERROR,
                "Does not handle changed watched files".to_string(),
            )
            .await;
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        if params.command == "load_workspace" {
            self.client
                .log_message(MessageType::INFO, "Loading entire workspace...".to_string())
                .await;
            self.load_workspace().await;
            self.client
                .log_message(MessageType::INFO, "Done loading!".to_string())
                .await;
        } else {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Unkown command: {}", params.command),
                )
                .await;
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
impl Name {
    fn module(&self) -> ast::Ud { self.1 }
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

#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
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
        Unknown(Scope, Option<ast::Ud>, ast::Ud, ast::Span),
        MultipleImports(BTreeSet<Name>, ast::Span),
        MultipleDefinitions(Name, (usize, usize), (usize, usize)),
        NotAConstructor(Name, ast::ProperName),
        NoConstructors(Name, ast::Span),
        ConstructorsDoesntExistOrIsntExported(ast::S<ast::Ud>, ast::S<ast::Ud>),
        NoConstructorOfThatName(ast::S<ast::Ud>, ast::Ud, ast::Ud, ast::Span),
        NotExportedOrDoesNotExist(ast::Ud, Scope, ast::Ud, ast::Span),
        CannotImportSelf(ast::Span),
        CouldNotFindImport(ast::Ud, ast::Span),
    }

    impl NRerrors {
        pub fn turn_into_diagnostic(
            self,
            names: &DashMap<ast::Ud, String>,
        ) -> tower_lsp::lsp_types::Diagnostic {
            match self {
                NRerrors::Unknown(scope, ns, n, span) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                    format!(
                        "Failed to resolve this name\n{:?} {}.{}\n{:?}.{:?}",
                        scope,
                        names
                            .get(&ns.unwrap_or(ast::Ud(0)))
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "_".into()),
                        names
                            .get(&n)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                        ns,
                        n
                    ),
                ),
                NRerrors::MultipleImports(ns, span) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                    format!(
                        "This name is imported from {} different modules\n",
                        ns.iter()
                            .map(|Name(s, m, n, _)| {
                                format!(
                                    "{:?} {}.{}",
                                    s,
                                    names
                                        .get(m)
                                        .map(|x| x.clone())
                                        .unwrap_or_else(|| "?".into()),
                                    names
                                        .get(n)
                                        .map(|x| x.clone())
                                        .unwrap_or_else(|| "?".into()),
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n")
                    ),
                ),
                NRerrors::MultipleDefinitions(Name(scope, _m, i, _), _first, second) => {
                    Diagnostic::new_simple(
                        Range::new(pos_from_tup(second), pos_from_tup(second)),
                        format!(
                            "{:?} {:?} is defined multiple times",
                            scope,
                            names
                                .get(&i)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into())
                        ),
                    )
                }
                NRerrors::NotAConstructor(d, m) => Diagnostic::new_simple(
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
                NRerrors::NoConstructors(m, s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    format!(
                        "{} does not have constructors",
                        names
                            .get(&m.2)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                    ),
                ),
                NRerrors::ConstructorsDoesntExistOrIsntExported(m, ast::S(u, s)) => {
                    Diagnostic::new_simple(
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
                    )
                }

                NRerrors::NoConstructorOfThatName(m, a, b, s) => Diagnostic::new_simple(
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
                ),
                NRerrors::NotExportedOrDoesNotExist(m, scope, ud, s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    format!(
                        "{:?} {}.{} is not exported or does not exist",
                        scope,
                        names
                            .get(&m)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                        names
                            .get(&ud)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                    ),
                ),
                NRerrors::CannotImportSelf(s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    "A module cannot import itself".to_string(),
                ),
                NRerrors::CouldNotFindImport(n, s) => Diagnostic::new_simple(
                    Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
                    format!(
                        "Could not find this import {}",
                        names
                            .get(&n)
                            .map(|x| x.clone())
                            .unwrap_or_else(|| "?".into()),
                    ),
                ),
            }
        }
    }

    #[derive(Debug)]
    pub struct N<'s> {
        pub me: ast::Ud,

        global_exports: &'s DashMap<ast::Ud, Vec<Export>>,
        pub global_usages: BTreeSet<(Name, ast::Span)>,

        // NOTE: Maybe this should be a `&mut DashMap<Ud, BTreeMap<Name, BTreeSet<ast::Span>>>` instead
        pub usages: BTreeMap<Name, BTreeSet<ast::Span>>,

        pub errors: Vec<NRerrors>,

        pub resolved: BTreeMap<(Pos, Pos), Name>,
        pub exports: Vec<Export>,

        constructors: BTreeMap<Name, BTreeSet<Name>>,

        pub defines: BTreeMap<Name, ast::Span>,
        locals: Vec<(Scope, ast::Ud, Name)>,
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
            self.locals.push((scope, u, name));

            self.def(s, name, false)
        }

        fn resolve(
            &mut self,
            scope: Scope,
            m: Option<ast::Ud>,
            n: ast::S<ast::Ud>,
        ) -> Option<Name> {
            let s = n.1;
            let n = n.0;
            let mut matches = Vec::new();
            matches.append(&mut self.resolve_inner(scope, m, n));

            let unique_matches = matches.into_iter().collect::<BTreeSet<_>>();
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
                    .push(NRerrors::MultipleImports(unique_matches.clone(), s)),
            }
            unique_matches.first().copied()
        }

        fn resolveq(
            &mut self,
            scope: Scope,
            m: Option<ast::Qual>,
            n: ast::S<ast::Ud>,
        ) -> Option<Name> {
            let m = m.map(|x| {
                self.resolve(Namespace, None, x.0);
                x.0 .0
            });
            self.resolve(scope, m, n)
        }

        fn find_local(&self, ss: Scope, n: ast::Ud) -> Option<Name> {
            self.locals
                .iter()
                .rfind(|(s, u, _)| *u == n && *s == ss)
                .map(|(_, _, name)| *name)
        }

        // For `A.B.C.foo` does `A.B.C` resolve to the module - or does it resolve to `foo`?
        fn resolve_inner(&self, ss: Scope, m: Option<ast::Ud>, n: ast::Ud) -> Vec<Name> {
            if m.is_none() {
                let locals: Vec<_> = [
                    self.find_local(ss, n),
                    if self
                        .defines
                        .contains_key(&Name(ss, self.me, n, Visibility::Public))
                    {
                        Some(Name(ss, self.me, n, Visibility::Public))
                    } else {
                        None
                    },
                ]
                .into_iter()
                .flatten()
                .collect();
                if locals.is_empty() {
                    self.imports
                        .get(&m)
                        .iter()
                        .flat_map(|x| {
                            x.values()
                                .flatten()
                                .flat_map(|i| i.to_names())
                                .find(|p| name_is(*p, ss, n))
                        })
                        .collect()
                } else {
                    locals
                }
            } else {
                self.imports
                    .get(&m)
                    .iter()
                    .flat_map(|x| {
                        x.values()
                            .flatten()
                            .flat_map(|i| i.to_names())
                            .find(|p| name_is(*p, ss, n))
                    })
                    .collect()
            }
        }

        fn export(&mut self, ex: &ast::Export) {
            // TODO: Resolve usages here so I can goto definition on things
            use Export::*;
            let export = match ex {
                ast::Export::Value(v) => {
                    self.resolve(Term, None, v.0);
                    Just(Name(Term, self.me, v.0 .0, Visibility::Public))
                }
                ast::Export::Symbol(v) => {
                    self.resolve(Term, None, v.0);
                    Just(Name(Term, self.me, v.0 .0, Visibility::Public))
                }
                ast::Export::Typ(v) => {
                    self.resolve(Type, None, v.0);
                    Just(Name(Type, self.me, v.0 .0, Visibility::Public))
                }
                ast::Export::TypSymbol(v) => {
                    self.resolve(Type, None, v.0);
                    Just(Name(Type, self.me, v.0 .0, Visibility::Public))
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
                    match ds {
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
                    }
                }
                ast::Export::Class(v) => {
                    self.resolve(Class, None, v.0);
                    Just(Name(Class, self.me, v.0 .0, Visibility::Public))
                }
                ast::Export::Module(v) => {
                    if let Some(ns) = self.imports.get(&Some(v.0 .0)).cloned() {
                        self.resolve(Namespace, None, v.0);
                        self.exports
                            .append(&mut ns.values().flatten().cloned().collect());
                        return;
                    } else {
                        let name = Name(Scope::Module, v.0 .0, v.0 .0, Visibility::Public);
                        self.resolved.insert((v.0 .1.lo(), v.0 .1.hi()), name);
                        // Module exports export everything that's ever imported from a module -
                        // right?
                        if let Some((_, is)) =
                            self.imports.values().flatten().find(|(x, _)| **x == v.0 .0)
                        {
                            self.exports.append(&mut is.clone())
                        } else {
                            self.errors.push(NRerrors::Unknown(
                                Scope::Module,
                                Some(v.0 .0),
                                v.0 .0,
                                v.0 .1,
                            ));
                        }
                        return;
                    }
                }
            };
            self.exports.push(export);
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
                                .any(|Name(s, _, n, _)| hiding.contains(&(*s, *n)))
                        })
                        .collect(),
                );
            } else {
                // TODO: I'm gonna need a test-suite for this
                // NOTE[et]: I've choosen to ignore hiding imports and re-exporting that module, because it really complicates things...
                let mut to_export = names
                    .iter()
                    .filter_map(|i| self.import_part(i, from.0.0, &exports))
                    .collect();
                self.imports
                    .get_mut(&import_name)
                    .expect("Checked earlier")
                    .get_mut(&from.0 .0)
                    .expect("Checked earlier")
                    .append(&mut to_export);
            }
        }

        fn import_part(
            &mut self,
            i: &ast::Import,
            from: ast::Ud,
            valid: &[Export],
        ) -> Option<Export> {
            let mut export_as = |scope: Scope, x: ast::Ud, s: ast::Span| -> Option<Export> {
                if let out @ Some(_) = valid.iter().find_map(|n| match n {
                    out @ Export::Just(name) if name_is(*name, scope, x) => Some(out),
                    _ => None,
                }) {
                    out.cloned()
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
                        out @ (Export::ConstructorsSome(name, _)
                        | Export::ConstructorsAll(name, _))
                            if name_is(*name, Type, x.0 .0) =>
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
                            if name_is(*name, Type, x.0 .0) =>
                        {
                            Some((name, cs))
                        }
                        _ => None,
                    }) {
                        let es = es
                            .iter()
                            .map(|n @ Name(_, _, x, _)| (x, n))
                            .collect::<BTreeMap<_, _>>();
                        let cs = cs.iter()
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
                            self.def_local(Type, n.0 .0, n.0 .1);
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
                    for b in bs.iter() {
                        self.binder(b);
                    }
                    self.guarded_expr(e);
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
            match b {
                ast::InstBinding::Sig(l, t) => {
                    if let Some(Name(_, m, _, _)) = u {
                        let span = l.0 .1;
                        self.resolved.insert(
                            (span.lo(), span.hi()),
                            Name(Term, m, l.0 .0, Visibility::Public),
                        );
                    }
                    self.typ(t);
                }
                ast::InstBinding::Def(l, binders, e) => {
                    if let Some(Name(_, m, _, _)) = u {
                        let span = l.0 .1;
                        self.resolved.insert(
                            (span.lo(), span.hi()),
                            Name(Term, m, l.0 .0, Visibility::Public),
                        );
                    }
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
                    let sf = self.push();
                    self.typ(t);
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
            for (k, vs) in grouped.iter() {
                for v in vs {
                    match v {
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
                    // Only define the first name for defs
                    if k.is_some() {
                        break;
                    }
                }
            }
            for vs in grouped.values() {
                let sf = self.push();
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
                        ast::LetBinding::Pattern(_, e) => {
                            self.expr(e);
                        }
                    }
                }
                self.pop(sf);
            }
        }

        fn binder(&mut self, b: &ast::Binder) {
            match b {
                ast::Binder::Typed(b, t) => {
                    self.binder(b);
                    let sf = self.push();
                    self.typ(t);
                    self.pop(sf);
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
                | ast::Typ::Int(_)
                | ast::Typ::Hole(_) => (),

                ast::Typ::Var(v) => {
                    if self.find_local(Type, v.0 .0).is_some() {
                        self.resolve(Type, None, v.0);
                    } else {
                        self.def_local(Type, v.0 .0, v.0 .1);
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
            }
        }
    }

    // Build a map of all source positions that have a name connected with them. We can then use
    // that mapping to update the global mapping.
    pub fn resolve_names(n: &mut N, prim: ast::Ud, m: &ast::Module) -> Option<ast::Ud> {
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
                let cs: BTreeSet<_> = n.constructors.values().flatten().collect();
                for name @ Name(s, m, _, v) in n.defines.keys() {
                    if *v != Visibility::Public {
                        continue;
                    }
                    if *m != n.me {
                        continue;
                    }
                    if !matches!(s, Scope::Class | Scope::Term | Scope::Type) {
                        continue;
                    }
                    if let Some(co) = n.constructors.get(name) {
                        n.exports.push(Export::ConstructorsAll(
                            *name,
                            co.iter().copied().collect::<Vec<_>>(),
                        ))
                    } else if !cs.contains(name) {
                        n.exports.push(Export::Just(*name))
                    }
                }
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
            self.client
                .log_message(
                    MessageType::ERROR,
                    &format!("Scanning: {:?}/lib/**/*.purs", folder.uri.to_string()),
                )
                .await;
            let mut deps: Vec<(ast::Ud, ast::Fi, BTreeSet<ast::Ud>, ast::Module)> = Vec::new();
            for path in glob(&format!(
                "{}/lib/**/*.purs",
                folder.uri.to_string().strip_prefix("file://")?
            ))
            .ok()?
            {
                let path = path.as_ref().ok()?;
                let x = (|| {
                    let source = std::fs::read_to_string(path.clone()).ok()?;
                    let url = Url::parse(&format!(
                        "file://{}",
                        &path.clone().into_os_string().into_string().ok()?
                    ))
                    .ok()?;
                    let (m, fi) = self.parse(url, None, &source);
                    let m = m?;
                    let (me, imports) = {
                        let header = m.0.clone()?;
                        let me = header.0 .0 .0;
                        (
                            me,
                            header
                                .2
                                .iter()
                                .map(|x| x.from.0 .0)
                                .collect::<BTreeSet<_>>(),
                        )
                    };
                    self.modules.insert(me, m.clone());
                    self.fi_to_ud.insert(fi, me);
                    self.ud_to_fi.insert(me, fi);
                    Some((me, fi, imports, m))
                })();
                if let Some(x) = x {
                    self.client
                        .log_message(
                            MessageType::ERROR,
                            &format!(
                                "DONE: {:?}",
                                &path.clone().into_os_string().into_string().ok()?
                            ),
                        )
                        .await;
                    deps.push(x);
                } else {
                    self.client
                        .log_message(
                            MessageType::ERROR,
                            &format!(
                                "FAILED: {:?}",
                                &path.clone().into_os_string().into_string().ok()?
                            ),
                        )
                        .await;
                }
            }
            self.client
                .log_message(MessageType::ERROR, &"========".to_string())
                .await;

            // NOTE: Not adding them to the name lookup
            fn h(s: &'static str) -> ast::Ud {
                let mut hasher = DefaultHasher::new();
                s.hash(&mut hasher);
                ast::Ud(hasher.finish() as usize)
            }

            let mut done: BTreeSet<_> = [
                h("Prim.Row"),
                h("Prim.Ordering"),
                h("Prim.RowList"),
                h("Prim.TypeError"),
                h("Prim.Boolean"),
                h("Prim.Coerce"),
                h("Prim.Symbol"),
            ]
            .into();
            loop {
                let todo: Vec<_> = deps
                    .iter()
                    .filter(|(m, _, deps, _)| (!done.contains(m)) && deps.is_subset(&done))
                    .collect();
                if todo.is_empty() {
                    if let Some((m, x)) = deps
                        .iter()
                        .map(|(m, _, deps, _)| {
                            (m, deps.difference(&done).cloned().collect::<Vec<_>>())
                        })
                        .min_by_key(|(_, aa)| match aa.len() {
                            0 => 9999,
                            n => n,
                        })
                    {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                &format!(
                                    "SMALLEST: {} [{}]",
                                    self.names.get(m).unwrap().value().clone(),
                                    x.iter()
                                        .map(|x| self.names.get(x).unwrap().value().clone())
                                        .collect::<Vec<_>>()
                                        .join(" ")
                                ),
                            )
                            .await;
                    }
                    self.client
                        .log_message(MessageType::ERROR, "EXITING!")
                        .await;
                    break;
                }
                for (_, fi, _, m) in todo.iter() {
                    self.resolve_module(m, *fi);
                    self.show_errors(*fi).await;
                }
                let names = todo
                    .iter()
                    .map(|(m, _, _, _)| self.names.get(m).unwrap().value().clone())
                    .collect::<Vec<_>>();
                self.client
                    .log_message(MessageType::ERROR, &format!("ROUND: {:?}", names))
                    .await;
                done.append(&mut todo.into_iter().map(|(m, _, _, _)| *m).collect());
            }
        }
        Some(())
    }

    fn resolve_module(&self, m: &ast::Module, fi: ast::Fi) -> Option<(bool, ast::Ud)> {
        let me = m.0.as_ref()?.0 .0 .0;
        let mut n = name_resolution::N::new(me, &self.exports);
        name_resolution::resolve_names(&mut n, self.prim, m);
        let me = n.me;

        self.name_resolution_errors.insert(
            fi,
            n.errors
                .into_iter()
                .map(|x| x.turn_into_diagnostic(&self.names))
                .collect::<Vec<_>>(),
        );

        self.resolved.insert(me, n.resolved);

        {
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

        {
            let new_imports: BTreeSet<_> =
                n.imports.values().flat_map(|x| x.values().flatten()).cloned().collect();
            let old_imports = self
                .imports
                .insert(me, new_imports.clone())
                .unwrap_or_else(BTreeSet::new);
            let new_imports = new_imports.iter().flat_map(|x| x.to_names().into_iter().map(|x| x.module())).collect::<BTreeSet<ast::Ud>>();
            let old_imports = old_imports.iter().flat_map(|x| x.to_names().into_iter().map(|x| x.module())).collect::<BTreeSet<ast::Ud>>();
            for x in old_imports.difference(&new_imports) {
                self.importers
                    .entry(*x)
                    .or_insert(BTreeSet::new())
                    .remove(&me);
            }
            for x in new_imports.difference(&old_imports) {
                self.importers
                    .entry(*x)
                    .or_insert(BTreeSet::new())
                    .insert(me);
            }
        }
        Some((exports_changed, n.me))
    }

    async fn resolve_cascading(&self, me: ast::Ud) {
        // TODO: This can be way way smarter, currently it only runs on changed exports.
        // Some exteions include: Lineage tracking - saying letting me know what parts actually
        // changed.
        let name = Name(Scope::Module, me, me, Visibility::Public);
        let mut checked = BTreeSet::new();
        let mut to_check: Vec<ast::Ud> = self
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
                if self
                    .exports
                    .get(&x)
                    .map(|ex| ex.iter().any(|x| x.contains(name)))
                    .unwrap_or(false)
                {
                    // It's a re-export which means we need to check everything that imports this as well!
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

    fn parse(
        &self,
        uri: Url,
        version: Option<i32>,
        source: &'_ str,
    ) -> (Option<ast::Module>, ast::Fi) {
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
        self.client
            .log_message(MessageType::ERROR, "GOT CHANGE!")
            .await;
        let (m, fi) = self.parse(params.uri.clone(), params.version, params.text);
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
                    let _ = self.client.workspace_diagnostic_refresh().await;
                }
            } else {
                self.show_errors(fi).await;
            }
        } else {
            self.show_errors(fi).await;
        }
        self.client
            .log_message(MessageType::ERROR, "FINISHED CHANGE!")
            .await;
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

fn build_builtins() -> (
    DashMap<ast::Ud, Vec<Export>>,
    ast::Ud,
    DashMap<ast::Ud, String>,
) {
    use Scope::*;
    let names = DashMap::new();

    let mut hasher = DefaultHasher::new();
    "Prim".hash(&mut hasher);
    let prim = ast::Ud(hasher.finish() as usize);

    let h = |a: Scope, n: &'static str, s: &'static str| -> (Scope, ast::Ud, ast::Ud) {
        let mut hasher = DefaultHasher::new();
        s.hash(&mut hasher);
        let s_ud = ast::Ud(hasher.finish() as usize);
        names.insert(s_ud, s.into());

        let mut hasher = DefaultHasher::new();
        n.hash(&mut hasher);
        let n_ud = ast::Ud(hasher.finish() as usize);
        names.insert(n_ud, n.into());
        (a, n_ud, s_ud)
    };

    let compiler_defines = [
        // https://pursuit.purerl.fun/builtins/docs/Prim
        h(Module, "Prim", "Prim"),
        h(Type, "Prim", "Int"),
        h(Type, "Prim", "Number"),
        h(Type, "Prim", "Record"),
        h(Type, "Prim", "Symbol"),
        h(Type, "Prim", "Array"),
        h(Type, "Prim", "Boolean"),
        h(Type, "Prim", "String"),
        h(Type, "Prim", "Char"),
        h(Type, "Prim", "->"),
        h(Type, "Prim", "Function"),
        h(Class, "Prim", "Partial"),
        h(Type, "Prim", "Type"),
        h(Type, "Prim", "Constraint"),
        h(Type, "Prim", "Symbol"),
        h(Type, "Prim", "Row"),
        //
        h(Module, "Prim.Boolean", "Prim.Boolean"),
        h(Type, "Prim.Boolean", "True"),
        h(Type, "Prim.Boolean", "False"),
        //
        h(Module, "Prim.Coerce", "Prim.Coerce"),
        h(Class, "Prim.Coerce", "Coercible"),
        //
        h(Module, "Prim.Ordering", "Prim.Ordering"),
        h(Type, "Prim.Ordering", "Ordering"),
        h(Type, "Prim.Ordering", "LT"),
        h(Type, "Prim.Ordering", "GT"),
        h(Type, "Prim.Ordering", "EQ"),
        //
        h(Module, "Prim.Row", "Prim.Row"),
        h(Class, "Prim.Row", "Union"),
        h(Class, "Prim.Row", "Nub"),
        h(Class, "Prim.Row", "Lacks"),
        h(Class, "Prim.Row", "Cons"),
        //
        h(Module, "Prim.RowList", "Prim.RowList"),
        h(Type, "Prim.RowList", "RowList"),
        h(Type, "Prim.RowList", "Cons"),
        h(Type, "Prim.RowList", "Nil"),
        h(Class, "Prim.RowList", "RowToList"),
        //
        h(Module, "Prim.Symbol", "Prim.Symbol"),
        h(Class, "Prim.Symbol", "Append"),
        h(Class, "Prim.Symbol", "Compare"),
        h(Class, "Prim.Symbol", "Cons"),
        //
        h(Module, "Prim.TypeError", "Prim.TypeError"),
        h(Class, "Prim.TypeError", "Warn"),
        h(Class, "Prim.TypeError", "Fail"),
        h(Type, "Prim.TypeError", "Doc"),
        h(Type, "Prim.TypeError", "Text"),
        h(Type, "Prim.TypeError", "Quote"),
        h(Type, "Prim.TypeError", "QuoteLabel"),
        h(Type, "Prim.TypeError", "Beside"),
        h(Type, "Prim.TypeError", "Above"),
        //
    ];

    let exports = DashMap::new();
    for (s, m, n) in compiler_defines {
        exports
            .entry(m)
            .or_insert(Vec::new())
            .push(Export::Just(Name(s, m, n, Visibility::Public)))
    }

    (exports, prim, names)
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (exports, prim, names) = build_builtins();

    let (service, socket) = LspService::build(|client| Backend {
        client,

        prim,
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

        exports,
        modules: DashMap::new(),
        resolved: DashMap::new(),
        defines: DashMap::new(),
        usages: DashMap::new(),

        syntax_errors: DashMap::new(),
        name_resolution_errors: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
