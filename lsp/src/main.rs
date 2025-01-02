#![feature(btree_cursors)]
#![allow(clippy::type_complexity)]

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Bound;
use std::sync::RwLock;

use dashmap::DashMap;
use futures::future::join_all;
use hemlis_lib::*;
use nr::{Export, NRerrors, Name, Pos, Scope, Visibility};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

macro_rules! or_ {
    ($e:expr, $b:block) => {
        if let Some(x) = $e {
            x
        } else $b
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,

    prim: ast::Ud,

    locked: RwLock<()>,
    has_started: RwLock<bool>,
    names: DashMap<ast::Ud, String>,

    fi_to_url: DashMap<ast::Fi, Url>,
    fi_to_ud: DashMap<ast::Fi, ast::Ud>,
    fi_to_source: DashMap<ast::Fi, String>,
    url_to_fi: DashMap<Url, ast::Fi>,
    fi_to_version: DashMap<ast::Fi, Option<i32>>,
    ud_to_fi: DashMap<ast::Ud, ast::Fi>,

    importers: DashMap<ast::Ud, BTreeSet<ast::Ud>>,
    imports: DashMap<ast::Ud, BTreeMap<Option<ast::Ud>, Vec<Export>>>,

    previouse_global_usages: DashMap<ast::Fi, BTreeSet<(Name, ast::Span, nr::Sort)>>,
    previouse_defines: DashMap<ast::Fi, BTreeSet<(Name, ast::Span)>>,

    // The option here should always be Some - but `None` lets us do a better partition search
    // here.
    available_locals: DashMap<ast::Fi, BTreeSet<(usize, usize, Option<Name>)>>,

    exports: DashMap<ast::Ud, Vec<Export>>,
    modules: DashMap<ast::Ud, ast::Module>,
    resolved: DashMap<ast::Ud, BTreeMap<(Pos, Pos), Name>>,
    defines: DashMap<Name, ast::Span>,
    usages: DashMap<ast::Ud, BTreeMap<Name, BTreeSet<(ast::Span, nr::Sort)>>>,

    syntax_errors: DashMap<ast::Fi, Vec<tower_lsp::lsp_types::Diagnostic>>,
    name_resolution_errors: DashMap<ast::Fi, Vec<tower_lsp::lsp_types::Diagnostic>>,
}

impl Backend {
    fn resolve_name(&self, url: &Url, pos: Position) -> Option<Name> {
        let m = self
            .fi_to_ud
            .try_get(&*self.url_to_fi.try_get(url).try_unwrap()?)
            .try_unwrap()?;
        let pos = (pos.line as usize, (pos.character + 1) as usize);
        let lut = self.resolved.try_get(&m).try_unwrap()?;
        let cur = lut.lower_bound(Bound::Included(&(pos, pos)));
        let ((lo, hi), name) = cur.peek_prev()?;
        if !(*lo <= pos && pos <= *hi) {
            return None;
        }
        Some(*name)
    }

    fn got_refresh(&self, fi: ast::Fi, version: Option<i32>) -> bool {
        match self.fi_to_version.try_get(&fi) {
            dashmap::try_result::TryResult::Present(x) => x.value() != &version,
            dashmap::try_result::TryResult::Absent => false,
            dashmap::try_result::TryResult::Locked => false,
        }
    }
}

fn try_line_to_offset(source: &str, line: usize, offset: usize) -> Option<&str> {
    let mut it = source.match_indices("\n");
    let (line_start, _) = it.nth(line.saturating_sub(1))?;
    let (line_end, _) = it.next()?;
    let end = (line_start + 1 + offset).max(line_end);
    let start = source[..end].rfind(char::is_whitespace)?;
    Some(&source[start + 1..end])
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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
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
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        {
            self.client
                .log_message(MessageType::INFO, hemlis_lib::version())
                .await;
            self.client
                .log_message(MessageType::INFO, "Scanning...".to_string())
                .await;
            self.load_workspace().await;
            self.client
                .log_message(MessageType::INFO, "Done scanning!".to_string())
                .await;
            let mut futures = Vec::new();
            for i in self.fi_to_url.iter() {
                futures.push(self.show_errors(*i.key(), None));
            }
            join_all(futures).await;
        }
        {
            let mut write = self.has_started.write().unwrap();
            *write = true;
        }
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
            let def_at = self.defines.try_get(&name).try_unwrap()?;
            let uri = self.fi_to_url.try_get(&def_at.fi()?).try_unwrap()?.clone();
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
                    .try_get(&name.1)
                    .try_unwrap()?
                    .get(&name)?
                    .iter()
                    .filter_map(|(s, sort): &(ast::Span, nr::Sort)| {
                        if !sort.is_def_or_ref() {
                            return None;
                        }
                        let url = self.fi_to_url.try_get(&s.fi()?).try_unwrap()?;
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

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let mut symbols = Vec::new();
        for i in self.previouse_defines.iter() {
            let fi = i.key();
            let names = i.value();
            for (Name(scope, ns, n, vis), at) in names.iter() {
                if *vis != Visibility::Public {
                    continue;
                }
                let n = self.names.try_get(n).try_unwrap().unwrap();
                let ns = self.names.try_get(ns).try_unwrap().unwrap();
                if Some(fi) != at.fi().as_ref() {
                    continue;
                };
                if !(n.value().starts_with(&params.query)
                    || ns.value().starts_with(&params.query)
                    || format!("{:?}", scope).starts_with(&params.query))
                {
                    continue;
                };
                let name = format!("{}.{}", ns.value(), n.value());
                #[allow(deprecated)]
                let out = SymbolInformation {
                    name,
                    kind: match scope {
                        Scope::Kind => SymbolKind::INTERFACE,
                        Scope::Type if n.starts_with(|c| matches!(c, 'a'..='z' | '_')) => {
                            SymbolKind::VARIABLE
                        }
                        Scope::Type => SymbolKind::INTERFACE,
                        Scope::Class => SymbolKind::CLASS,
                        Scope::Term if n.starts_with(|c| matches!(c, 'A'..='Z' | '_')) => {
                            SymbolKind::CONSTRUCTOR
                        }
                        Scope::Term => SymbolKind::FUNCTION,
                        Scope::Module => SymbolKind::MODULE,
                        Scope::Namespace => SymbolKind::NAMESPACE,
                    },
                    tags: None,

                    deprecated: None,

                    location: {
                        let url = self.fi_to_url.try_get(fi).unwrap().clone();
                        let lo = pos_from_tup(at.lo());
                        let hi = pos_from_tup(at.hi());

                        let range = Range::new(lo, hi);

                        Location::new(url, range)
                    },

                    container_name: None,
                };
                symbols.push(out);
            }
        }
        Ok(Some(symbols))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let mut symbols = Vec::new();
        let fi_inner = if let Some(fi) = self
            .url_to_fi
            .try_get(&params.text_document.uri)
            .try_unwrap()
        {
            *fi
        } else {
            return Err(Error::new(ErrorCode::ServerError(1)));
        };
        if let Some(names) = self.previouse_defines.try_get(&fi_inner).try_unwrap() {
            for (Name(scope, _, n, vis), at) in names.iter() {
                if *vis != Visibility::Public {
                    continue;
                }
                let fi = if let Some(fi) = at.fi() { fi } else { continue };
                let name = match self.names.try_get(n).try_unwrap() {
                    Some(n) => n.clone(),
                    None => continue,
                };
                #[allow(deprecated)]
                let out = SymbolInformation {
                    name: name.clone(),
                    kind: match scope {
                        Scope::Kind => SymbolKind::INTERFACE,
                        Scope::Type if name.starts_with(|c| matches!(c, 'a'..='z' | '_')) => {
                            SymbolKind::VARIABLE
                        }
                        Scope::Type => SymbolKind::INTERFACE,
                        Scope::Class => SymbolKind::CLASS,
                        Scope::Term if name.starts_with(|c| matches!(c, 'A'..='Z' | '_')) => {
                            SymbolKind::CONSTRUCTOR
                        }
                        Scope::Term => SymbolKind::FUNCTION,
                        Scope::Module => SymbolKind::MODULE,
                        Scope::Namespace => SymbolKind::NAMESPACE,
                    },
                    tags: None,
                    deprecated: None,

                    location: {
                        let url = self.fi_to_url.try_get(&fi).unwrap().clone();
                        let lo = pos_from_tup(at.lo());
                        let hi = pos_from_tup(at.hi());

                        let range = Range::new(lo, hi);

                        Location::new(url, range)
                    },

                    container_name: None,
                };
                symbols.push(out);
            }
        }
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let completions = || -> Option<Vec<CompletionItem>> {
            let fi = *self.url_to_fi.try_get(&uri).try_unwrap()?;
            let me = *self.fi_to_ud.try_get(&fi).try_unwrap()?;
            let source = self.fi_to_source.try_get(&fi).try_unwrap()?;
            let line = position.line as usize;
            let to_complete =
                try_line_to_offset(&source, line, position.character as usize)?.to_string();
            drop(source);

            let maybe_first_letter = to_complete.chars().nth(0);

            let mut out = Vec::new();
            // Locals
            let lut = self.available_locals.try_get(&fi).try_unwrap()?;
            let mut cur = lut.lower_bound(Bound::Included(&(0, line, None)));
            while let Some((l, h, n)) = cur.peek_next() {
                let n = n.expect("This option is only for searching!");
                if !(*l <= line && line <= *h) {
                    break;
                }

                let name = or_!(self.names.try_get(&n.name()).try_unwrap(), { continue });
                if maybe_first_letter.is_none() || name.starts_with(maybe_first_letter.unwrap()) {
                    out.push(CompletionItem::new_simple(
                        name.value().to_string(),
                        format!("{:?}", n.scope()),
                    ));
                }

                cur.next();
            }

            // Imported
            let (namespace, var) = if let Some(last) = to_complete.rfind('.') {
                (
                    Some(ast::Ud::new(to_complete[..last - 1].into())),
                    to_complete[last..].into(),
                )
            } else {
                (None, to_complete.clone())
            };
            if let Some(imports) = self
                .imports
                .try_get(&me)
                .try_unwrap()
                .and_then(|x| x.get(&namespace).cloned())
            {
                for n in imports.iter().flat_map(|x| x.to_names()) {
                    if let Some(name) = self.names.try_get(&n.name()).try_unwrap() {
                        if name.value().starts_with(&var) {
                            out.push(CompletionItem::new_simple(
                                name.value().to_string(),
                                format!("{:?}", n.scope()),
                            ));
                        }
                    }
                }
            }

            // Defines
            for x in self.defines.iter() {
                let n = x.key();
                if n.module() != me {
                    continue;
                }
                let module = or_!(self.names.try_get(&n.module()).try_unwrap(), { continue });
                let name = or_!(self.names.try_get(&n.name()).try_unwrap(), { continue });
                if maybe_first_letter.is_none()
                    || module.starts_with(maybe_first_letter.unwrap())
                    || name.starts_with(maybe_first_letter.unwrap())
                {
                    out.push(CompletionItem::new_simple(
                        format!("{}.{}", module.value(), name.value()),
                        format!("{:?}", n.scope()),
                    ));
                }
            }

            Some(out)
            // self.defines
            //
            // - Find the word under the cursor
            // - Are we completing a Module - what modules are in scope?
            // - Are we completing a variable - what variables are in scope?
            // - Are we ccompleting an import - what imports match?
        }();
        Ok(completions.map(CompletionResponse::Array))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = (|| {
            let name = self.resolve_name(
                &params.text_document_position.text_document.uri,
                params.text_document_position.position,
            )?;
            let mut edits = HashMap::new();
            for at in self
                .usages
                .try_get(&name.1)
                .try_unwrap()?
                .get(&name)?
                .iter()
                .map(|(at, _)| *at)
                .collect::<BTreeSet<_>>()
                .into_iter()
            {
                let url = self
                    .fi_to_url
                    .try_get(&if let Some(fi) = at.fi() { fi } else { continue })
                    .try_unwrap()?
                    .clone();
                let lo = pos_from_tup(at.lo());
                let hi = pos_from_tup(at.hi());
                let range = Range::new(lo, hi);
                edits.entry(url).or_insert(Vec::new()).push(TextEdit {
                    range,
                    new_text: params.new_name.clone(),
                });
            }
            Some(WorkspaceEdit::new(edits))
        })();
        Ok(workspace_edit)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(
                MessageType::INFO,
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
                    MessageType::INFO,
                    format!("Unkown command: {}", params.command),
                )
                .await;
        }
        Ok(None)
    }
}

fn create_error(
    range: Range,
    code: String,
    message: String,
    related: Vec<(String, Location)>,
) -> tower_lsp::lsp_types::Diagnostic {
    Diagnostic::new(
        range,
        Some(DiagnosticSeverity::ERROR),
        Some(NumberOrString::String(code)),
        Some("hemlis".into()),
        message,
        {
            let related: Vec<_> = related
                .into_iter()
                .map(|(message, location)| DiagnosticRelatedInformation { message, location })
                .collect();
            if related.is_empty() {
                None
            } else {
                Some(related)
            }
        },
        None,
    )
}

fn create_warning(
    range: Range,
    code: String,
    message: String,
    related: Vec<(String, Location)>,
) -> tower_lsp::lsp_types::Diagnostic {
    Diagnostic::new(
        range,
        Some(DiagnosticSeverity::WARNING),
        Some(NumberOrString::String(code)),
        Some("hemlis".into()),
        message,
        {
            let related: Vec<_> = related
                .into_iter()
                .map(|(message, location)| DiagnosticRelatedInformation { message, location })
                .collect();
            if related.is_empty() {
                None
            } else {
                Some(related)
            }
        },
        None,
    )
}

pub fn nrerror_turn_into_diagnostic(
    error: NRerrors,
    names: &DashMap<ast::Ud, String>,
) -> tower_lsp::lsp_types::Diagnostic {
    match error {
        NRerrors::Unknown(scope, ns, n, span) => create_error(
            Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
            "Unknown".into(),
            format!(
                "{:?} {}\nFailed to resolve",
                scope,
                match (
                    names
                        .try_get(&ns.unwrap_or(ast::Ud(0, false)))
                        .try_unwrap()
                        .map(|x| x.clone()),
                    names.try_get(&n).try_unwrap().map(|x| x.clone())
                ) {
                    (None, Some(name)) => name,
                    (Some(ns), Some(name)) => format!("{}.{}", ns, name),
                    (_, _) => "!! UNREACHABLE - PLEASE REPORT".into(),
                }
            ),
            Vec::new(),
        ),
        NRerrors::NameConflict(ns, span) => create_error(
            Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
            "NameConflict".into(),
            format!(
                "This name is imported from {} different modules\n{}",
                ns.len(),
                ns.iter()
                    .map(|Name(s, m, n, x)| {
                        format!(
                            "{:?} {}.{} {:?}",
                            s,
                            names
                                .try_get(m)
                                .try_unwrap()
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            names
                                .try_get(n)
                                .try_unwrap()
                                .map(|x| x.clone())
                                .unwrap_or_else(|| "?".into()),
                            x,
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Vec::new(),
        ),
        NRerrors::MultipleDefinitions(Name(scope, _m, i, _), _first, second) => create_error(
            Range::new(pos_from_tup(second), pos_from_tup(second)),
            "MultipleDefinitions".into(),
            format!(
                "{:?} {:?} is defined multiple times",
                scope,
                names
                    .try_get(&i)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into())
            ),
            Vec::new(),
        ),
        NRerrors::NotAConstructor(d, m) => create_error(
            Range::new(pos_from_tup(m.0 .1.lo()), pos_from_tup(m.0 .1.hi())),
            "NotAConstructor".into(),
            format!(
                "{} does not have a constructors {}",
                names
                    .try_get(&d.2)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into()),
                names
                    .try_get(&m.0 .0)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into())
            ),
            Vec::new(),
        ),
        NRerrors::NoConstructors(m, s) => create_error(
            Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
            "NoConstructors".into(),
            format!(
                "{} does not have constructors",
                names
                    .try_get(&m.2)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into()),
            ),
            Vec::new(),
        ),
        NRerrors::NotExportedOrDoesNotExist(m, scope, ud, s) => create_error(
            Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
            "NotExportedOrDoesNotExist".into(),
            format!(
                "{:?} {}.{} is not exported or does not exist",
                scope,
                names
                    .try_get(&m)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into()),
                names
                    .try_get(&ud)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into()),
            ),
            Vec::new(),
        ),
        NRerrors::CannotImportSelf(s) => create_error(
            Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
            "CannotImportSelf".into(),
            "A module cannot import itself".to_string(),
            Vec::new(),
        ),
        NRerrors::CouldNotFindImport(n, s) => create_error(
            Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
            "CouldNotFindImport".into(),
            format!(
                "Could not find this import {}",
                names
                    .try_get(&n)
                    .try_unwrap()
                    .map(|x| x.clone())
                    .unwrap_or_else(|| "?".into()),
            ),
            Vec::new(),
        ),
        NRerrors::Unused(info, s) => create_warning(
            Range::new(pos_from_tup(s.lo()), pos_from_tup(s.hi())),
            "Unused".into(),
            info,
            Vec::new(),
        ),
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
    async fn log(&self, s: String) {
        self.client.log_message(MessageType::INFO, s).await;
    }

    async fn load_workspace(&self) -> Option<()> {
        self.log("LOAD_WORKSPACE - START".into()).await;
        let folders = self.client.workspace_folders().await.ok()??;

        for folder in folders {
            use glob::glob;
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
                    let uri = Url::parse(&format!(
                        "file://{}",
                        &path.clone().into_os_string().into_string().ok()?
                    ))
                    .ok()?;
                    let fi = self.find_fi(uri.clone()).unwrap();
                    self.fi_to_source.insert(fi, source.to_string());
                    self.fi_to_url.insert(fi, uri.clone());
                    self.fi_to_version.insert(fi, None);
                    let (m, fi) = self.parse(fi, &source);
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
            self.log("========".into()).await;

            // NOTE: Not adding them to the name lookup
            fn h(s: &'static str) -> ast::Ud {
                let mut hasher = DefaultHasher::new();
                s.hash(&mut hasher);
                ast::Ud(hasher.finish() as usize, s.starts_with("_"))
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
                    let left: Vec<_> = deps
                        .iter()
                        .filter(|(m, _, _, _)| !done.contains(m))
                        .collect();
                    if !left.is_empty() {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                &format!("Dependency cycle detected: {}", left.len()),
                            )
                            .await;
                    }
                    break;
                }
                let mut futures = Vec::new();
                for (_, fi, _, m) in todo.iter() {
                    futures.push(async {
                        self.resolve_module(m, *fi, None);
                    });
                }
                join_all(futures).await;
                done.append(&mut todo.into_iter().map(|(m, _, _, _)| *m).collect());
            }
        }
        self.log("LOAD_WORKSPACE - DONE".into()).await;
        Some(())
    }

    fn resolve_module(
        &self,
        m: &ast::Module,
        fi: ast::Fi,
        version: Option<i32>,
    ) -> Option<(bool, ast::Ud)> {
        let me = m.0.as_ref()?.0 .0 .0;
        let mut n = nr::N::new(me, &self.exports);
        nr::resolve_names(&mut n, self.prim, m);

        let nr::N {
            me,
            errors,
            resolved,
            usages,
            defines,
            global_usages,
            exports,
            ..
        } = n;

        if self.got_refresh(fi, version) {
            return None;
        }
        let lock = self.locked.write();
        if self.got_refresh(fi, version) {
            return None;
        }

        self.modules.insert(me, m.clone());
        self.fi_to_ud.insert(fi, me);
        self.ud_to_fi.insert(me, fi);

        self.name_resolution_errors.insert(
            fi,
            errors
                .into_iter()
                .map(|x| nrerror_turn_into_diagnostic(x, &self.names))
                .collect::<Vec<_>>(),
        );

        self.resolved.insert(me, resolved);

        {
            let mut us = self.usages.entry(me).or_insert(BTreeMap::new());
            for (k, v) in us.iter_mut() {
                v.retain(|(x, _)| x.fi() != Some(fi));
                v.append(&mut usages.get(k).cloned().unwrap_or_default());
            }
            for (k, v) in usages.into_iter() {
                if us.contains_key(&k) {
                    continue;
                }
                assert!(us.insert(k, v).is_none());
            }
        }

        {
            let defined_scopes = defines
                .iter()
                .filter_map(|(a, (_, available_in))| {
                    available_in.map(|x| (x.lines().0, x.lines().1, Some(*a)))
                })
                .collect::<BTreeSet<_>>();
            self.available_locals.insert(fi, defined_scopes);
        }

        {
            let new = defines
                .into_iter()
                .map(|(a, (at, _))| (a, at))
                .collect::<BTreeSet<_>>();
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
            let new = global_usages
                .into_iter()
                .flat_map(|(name, xx)| xx.into_iter().map(move |(pos, sort)| (name, pos, sort)))
                .collect::<BTreeSet<_>>();
            let old = self
                .previouse_global_usages
                .insert(fi, new.clone())
                .unwrap_or_default();
            for (name, pos, sort) in old.difference(&new) {
                if let Some(mut e) = self.usages.get_mut(&name.1) {
                    if let Some(e) = e.get_mut(name) {
                        e.remove(&(*pos, *sort));
                    }
                }
            }

            for (name, pos, sort) in new.difference(&old) {
                let mut e = self.usages.entry(name.1).or_insert(BTreeMap::new());
                e.entry(*name)
                    .or_insert(BTreeSet::new())
                    .insert((*pos, *sort));
            }
        }

        let exports_changed = {
            let new_hash = hash_exports(&exports);
            if let Some(old) = self.exports.insert(me, exports) {
                new_hash != hash_exports(&old)
            } else {
                true
            }
        };

        {
            let new_imports: BTreeMap<_, _> = n
                .imports
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().flat_map(|(_, x)| x).collect::<Vec<_>>()))
                .collect();
            let old_imports = self
                .imports
                .insert(me, new_imports.clone())
                .unwrap_or_else(BTreeMap::new);
            let new_imports = new_imports
                .values()
                .flat_map(|x| {
                    x.iter()
                        .flat_map(|x| x.to_names().into_iter().map(|x| x.module()))
                })
                .collect::<BTreeSet<ast::Ud>>();
            let old_imports = old_imports
                .values()
                .flat_map(|x| {
                    x.iter()
                        .flat_map(|x| x.to_names().into_iter().map(|x| x.module()))
                })
                .collect::<BTreeSet<ast::Ud>>();
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
        drop(lock);
        Some((exports_changed, n.me))
    }

    async fn resolve_cascading(&self, me: ast::Ud, fi: ast::Fi, version: Option<i32>) {
        // TODO: This can be way way smarter, currently it only runs on changed exports.
        // Some exteions include: Lineage tracking - saying letting me know what parts actually
        // changed.
        let name = Name(Scope::Module, me, me, Visibility::Public);
        let mut checked = BTreeSet::new();
        let mut to_check: Vec<ast::Ud> = self
            .importers
            .try_get(&me)
            .try_unwrap()
            .iter()
            .flat_map(|x| x.iter())
            .copied()
            .collect();
        while let Some(x) = to_check.pop() {
            if self.got_refresh(fi, version) {
                return;
            }
            if checked.contains(&x) {
                continue;
            }
            checked.insert(x);
            if let (Some(m), Some(fi), Some(version)) = (
                self.modules.try_get(&x).try_unwrap(),
                self.ud_to_fi.try_get(&x).try_unwrap(),
                self.fi_to_version.try_get(&fi).try_unwrap(),
            ) {
                let _ = self.resolve_module(&m, *fi, *version);
                if self.got_refresh(*fi, *version) {
                    return;
                }
                if self
                    .exports
                    .try_get(&x)
                    .try_unwrap()
                    .map(|ex| ex.iter().any(|x| x.contains(name)))
                    .unwrap_or(false)
                {
                    // It's a re-export which means we need to check everything that imports this as well!
                    to_check.append(
                        &mut self
                            .importers
                            .try_get(&x)
                            .try_unwrap()
                            .iter()
                            .flat_map(|x| x.iter())
                            .copied()
                            .collect::<Vec<_>>(),
                    );
                }
            }
        }
    }

    async fn show_errors(&self, fi: ast::Fi, version: Option<i32>) {
        if self.got_refresh(fi, version) {
            return;
        }
        if let Some(url) = self.fi_to_url.try_get(&fi).try_unwrap() {
            self.client
                .publish_diagnostics(
                    url.clone(),
                    [
                        if let Some(x) = self.syntax_errors.try_get(&fi).try_unwrap() {
                            x.value().clone()
                        } else {
                            Vec::new()
                        },
                        if let Some(x) = self.name_resolution_errors.try_get(&fi).try_unwrap() {
                            x.value().clone()
                        } else {
                            Vec::new()
                        },
                    ]
                    .concat(),
                    if let Some(v) = self.fi_to_version.try_get(&fi).try_unwrap() {
                        *v
                    } else {
                        None
                    },
                )
                .await
        }
    }

    fn parse(&self, fi: ast::Fi, source: &'_ str) -> (Option<ast::Module>, ast::Fi) {
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

                    create_error(
                        Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                        "Syntax".into(),
                        message,
                        Vec::new(),
                    )
                })
                .collect::<Vec<_>>(),
        );
        (m, fi)
    }

    fn find_fi(&self, uri: Url) -> Option<ast::Fi> {
        match self.url_to_fi.try_entry(uri.clone()) {
            Some(dashmap::Entry::Occupied(v)) => Some(*v.get()),
            Some(dashmap::Entry::Vacant(v)) => {
                let fi = ast::Fi(sungod::Ra::ggen::<usize>());
                v.insert(fi);
                Some(fi)
            }
            None => None,
        }
    }

    async fn on_change(&self, params: TextDocumentItem<'_>) {
        if !self.has_started.try_read().map(|x| *x).unwrap_or(false) {
            self.client
                .log_message(MessageType::INFO, "Blocking since not started")
                .await;
            return;
        }
        let uri = params.uri.clone();
        let source = params.text;
        let version = params.version;

        self.log(format!(
            "!! {:?} CHANGE! {:?}",
            params.version,
            params.uri.to_string()
        ))
        .await;

        let fi = loop {
            if let Some(fi) = self.find_fi(uri.clone()) {
                break fi;
            }
            self.client
                .log_message(MessageType::ERROR, "FAILED TO GENERATE FI")
                .await;
        };
        // TODO: How to handle two files with the same module name?
        match self.fi_to_version.try_entry(fi) {
            Some(dashmap::Entry::Occupied(mut o)) => {
                if o.get().is_some() && o.get() > &version {
                    return;
                }
                o.insert(version);
            }
            Some(dashmap::Entry::Vacant(v)) => {
                v.insert(version);
            }
            None => return,
        }
        if self.got_refresh(fi, version) {
            return;
        }

        self.log(format!(
            "!! {:?} AA! {:?}",
            params.version,
            params.uri.to_string()
        ))
        .await;

        // I have to copy it! :(
        self.fi_to_source.insert(fi, source.to_string());
        self.fi_to_url.insert(fi, uri.clone());
        let (m, fi) = self.parse(fi, source);

        self.log(format!(
            "!! {:?} BB! {:?}",
            params.version,
            params.uri.to_string()
        ))
        .await;

        // TODO: We could exit earlier if we have the same syntactical structure here
        if let Some(m) = m {
            self.log(format!(
                "!! {:?} PRE RESOLVE! {:?}",
                params.version,
                params.uri.to_string()
            ))
            .await;
            if let Some((exports_changed, me)) = self.resolve_module(&m, fi, params.version) {
                self.log(format!(
                    "!! {:?} POST RESOLVE! {:?}",
                    params.version,
                    params.uri.to_string()
                ))
                .await;

                if self.got_refresh(fi, version) {
                    return;
                }

                if exports_changed {
                    self.log("CASCADE CHANGE - START".into()).await;
                    self.resolve_cascading(me, fi, params.version).await;
                    let _ = self.client.workspace_diagnostic_refresh().await;
                    self.log("CASCADE CHANGE - END".into()).await;
                }
                self.show_errors(fi, params.version).await;
            } else {
                self.show_errors(fi, params.version).await;
            }
        } else {
            self.show_errors(fi, params.version).await;
        }
        self.log(format!(
            "!! {:?} FINISHED! {:?}",
            params.version,
            params.uri.to_string()
        ))
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    println!("{}", hemlis_lib::version());

    let (exports, prim, names) = hemlis_lib::build_builtins();

    let (service, socket) = LspService::build(|client| Backend {
        client,

        locked: ().into(),
        has_started: false.into(),
        prim,
        names,

        fi_to_url: DashMap::new(),
        fi_to_ud: DashMap::new(),
        fi_to_source: DashMap::new(),
        ud_to_fi: DashMap::new(),
        url_to_fi: DashMap::new(),
        fi_to_version: DashMap::new(),

        importers: DashMap::new(),
        imports: DashMap::new(),

        available_locals: DashMap::new(),

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
