use dashmap::DashMap;
use log::{debug, error};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use purring_lib::*;

#[derive(Debug)]
struct Backend {
    client: Client,
    names: DashMap<ast::Ud, String>,
    url_to_fi: DashMap<String, ast::Fi>,
    modules: DashMap<ast::Fi, ast::Module>,
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
        error!("GOTODEF");
        let definition = || -> Option<GotoDefinitionResponse> {
            let fd = *self.url_to_fi.get(&params.text_document_position_params.text_document.uri.to_string())?;
            // NOTE: might have an off-by-one
            let pos = params.text_document_position_params.position;
            let pos = (pos.line as usize, pos.character as usize);
            // let m = self.modules.get(&fd)?;
            None

            // let spot = goto_def::search(&m, pos);
            // error!("Searching for: {:?}, got: {:?}", pos, spot);
            // match spot{
            //     goto_def::Definition::Local(s) =>
            //             Some(GotoDefinitionResponse::Scalar(Location { 
            //                     uri: params.text_document_position_params.text_document.uri,
            //                     range: Range { start: pos_from_tup(s.lo()), end: pos_from_tup(s.hi()) },

            //             })),
            //     _ => None
            // }
        }();
        Ok(definition)
    }

    /*
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        //let reference_list = || -> Option<Vec<Location>> {
        //    let uri = params.text_document_position.text_document.uri;
        //    let semantic = self.semantic_map.get(uri.as_str())?;
        //    let rope = self.document_map.get(uri.as_str())?;
        //    let position = params.text_document_position.position;
        //    let offset = position_to_offset(position, &rope)?;
        //    let reference_span_list = get_references(&semantic, offset, offset + 1, false)?;

        //    let ret = reference_span_list
        //        .into_iter()
        //        .filter_map(|range| {
        //            let start_position = offset_to_position(range.start, &rope)?;
        //            let end_position = offset_to_position(range.end, &rope)?;

        //            let range = Range::new(start_position, end_position);

        //            Some(Location::new(uri.clone(), range))
        //        })
        //        .collect::<Vec<_>>();
        //    Some(ret)
        //}();
        //Ok(reference_list)
    }
    */

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

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}


pub type Pos = (usize, usize);

pub type Name = (ast::Ud, ast::Ud, usize);

mod name_resolution {
    use std::collections::{BTreeMap, BTreeSet};
    use super::*;

    use purring_lib::{ast, lexer, parser};

    #[derive(Debug)]
    pub enum NRerrors {
        Unknown(ast::Span),
        Multiple(Vec<(ast::Ud, ast::Span)>, ast::Span),
    }

    #[derive(Debug)]
    pub struct N<'s> {
            me: ast::Ud,
            known: &'s BTreeMap<Name, BTreeSet<Pos>>, 
            exports: &'s mut Vec<Name>,

            errors: &'s mut Vec<NRerrors>,

            resolved: &'s mut BTreeMap<Pos, Name>,
            module_imports: &'s mut BTreeMap<ast::Ud, Vec<(ast::Ud, ast::Span)>>,
            imports: &'s mut BTreeMap<ast::Ud, Name>,

            globals: &'s mut BTreeMap<Name, Pos>,
            locals: &'s mut Vec<(ast::Ud, Name)>,

            Are types and terms in different namespaces? I'm pretty sure they are... 
    }

    impl<'s> N<'s> {
        fn def_global(&mut self, s: &ast::S<ast::Ud>) {
            let name = (self.me, s.0, 0);
            self.globals.insert(name, s.1.lo());
            self.resolved.insert(s.1.lo(), name);
        }

        fn def_global_(&mut self, s: &ast::S<ast::Ud>) {
            let name = (self.me, s.0, 0);
            self.globals.insert(name, s.1.lo());
            self.resolved.insert(s.1.lo(), name);
        }

        fn def_local(&mut self, s: &ast::S<ast::Ud>) {
            let name = (self.me, s.0, s.1.lo().0);
            self.locals.push((s.0, name));
            self.resolved.insert(s.1.lo(), name);
        }

        fn def_import(&self, a: ast::Ud, x: ast::S<ast::Ud>) {
        }

        fn resolve(&mut self, m: ast::Ud, n: ast::Ud, s: ast::Span) {
            let mut matches = Vec::new();
            for mm in self.module_imports.get(&m).iter().map(|x|x.iter()).flatten() {
                if let Some(name) = self.resolve_inner(mm.0, n) {
                    self.resolved.insert(s.lo(), name);
                    matches.push(*mm);
                }
            }
            match matches.len() {
                0 => self.errors.push(NRerrors::Unknown(s)),
                1 => (),
                _ => self.errors.push(NRerrors::Multiple(matches, s)),
            }
        }

        // For `A.B.C.foo` does `A.B.C` resolve to the module - or does it resolve to `foo`?
        fn resolve_(&mut self, n: ast::S<ast::Ud>) {
            self.resolve(self.me, n.0, n.1)
        }

        fn resolve_inner(&self, m: ast::Ud, n: ast::Ud) -> Option<Name> {
            if m == self.me {
                if let Some((_, name)) = self.locals.iter().rfind(|(u, _)| *u == n) {
                    return Some(*name);
                }
            }

            if self.globals.get(&(m, n, 0)).is_some() {
                return Some((m, n, 0));
            }

            if m == self.me {
                if let Some(name) = self.imports.get(&n) {
                    return Some(*name);
                }
            }

            None
        }

    }

    // Build a map of all source positions that have a name connected with them. We can then use
    // that mapping to update the global mapping. 
    pub fn resolve(m: &ast::Module, n: &mut N<'_>) -> Option<ast::Ud>{
        if let Some(h) = m.0.as_ref() {
            let name = mname(&h.0, n);
            for i in h.2.iter() {
                import(i, n);
            }
            for d in m.1.iter() {
                decl(d, n);
            }

            let exports = h.1;

            Some(h.0.0.0)
        } else {
            None
        }
    }

    fn import(i: &ast::ImportDecl, n: &mut N)  {
        match i {
            ast::ImportDecl::As(a, b) => {
                match n.module_imports.entry(a.0.0) {
                    std::collections::btree_map::Entry::Vacant(v) => { v.insert(vec![(b.0.0, b.0.1)]); },
                    std::collections::btree_map::Entry::Occupied(v) => { v.get_mut().push(((b.0.0, b.0.1))); },
                }
            },
            ast::ImportDecl::Multiple(a, imports) => {
                for i in imports {
                    match i {
                        ast::Import::Value(x) => {
                            n.def_import(a.0.0, x.0)
                        },
                        ast::Import::Symbol(x) => {
                            n.def_import(a.0.0, x.0)
                        }
                        ast::Import::Typ(x) => {
                            n.def_import(a.0.0, x.0)
                        }
                        ast::Import::TypDat(x, _) => {
                            // TODO: The data-members
                            n.def_import(a.0.0, x.0)
                        }
                        ast::Import::TypSymbol(x) => {
                            n.def_import(a.0.0, x.0)
                        }
                        ast::Import::Class(x) => {
                            n.def_import(a.0.0, x.0)
                        }
                    }
                }
                a.0.0
            },
            ast::ImportDecl::Hiding(_, _) => todo!(),
            ast::ImportDecl::Bulk(_) => todo!(),
        }
    }

    fn mname(h: &ast::MName, n: &mut N<'_>) -> ast::Ud {
        n.def(h.0);
        h.0.0
    }

    fn header(m: &ast::Header, n: &mut N<'_>) {
        todo!()
    }

    fn decl(d: &ast::Decl, n: &mut N<'_>) {
        todo!()
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
        let l = lexer::lex(params.text, ast::Fi(0));
        let mut p = parser::P::new(&l, &self.names);
        if let Some(m) = parser::module(&mut p) {
            self.modules
                .insert(fi, m);
        };

        let diagnostics = p.errors
            .into_iter()
            .map(|err| {
                let message = match err {
                    parser::Serror::Info(_, s) => 
                        format!("Info: {}", s),
                    parser::Serror::Unexpected(_, t, s) => 
                        format!("Unexpected {:?}: {}", t, s),
                    parser::Serror::NotSimpleTypeVarBinding(_) => 
                        format!("Not a simple type-var binding"),
                    parser::Serror::NotAConstraint(_) => 
                        format!("Not a constraint"),
                    parser::Serror::NotAtEOF(_, _) => 
                        format!("Not at end of file"),
                    parser::Serror::FailedToParseDecl(_, _, _, _) => 
                        format!("Failed to parse this declaration"),
                };
                let span = err.span();

                Diagnostic::new_simple(
                    Range::new(pos_from_tup(span.lo()), pos_from_tup(span.hi())),
                    message,
                )
            })
            .collect::<Vec<_>>();


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
        url_to_fi: DashMap::new(),
        modules: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

