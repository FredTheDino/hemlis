use dashmap::DashMap;
use log::debug;
use ropey::Rope;
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
                execute_command_provider: None,
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: None,
                // definition: Some(GotoCapability::default()),
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

    /*
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            //let uri = params.text_document_position_params.text_document.uri;
            //let semantic = self.semantic_map.get(uri.as_str())?;
            //let rope = self.document_map.get(uri.as_str())?;
            //let position = params.text_document_position_params.position;
            //let offset = position_to_offset(position, &rope)?;

            //let interval = semantic.ident_range.find(offset, offset + 1).next()?;
            //let interval_val = interval.val;
            //let range = match interval_val {
            //    IdentType::Binding(symbol_id) => {
            //        let span = &semantic.table.symbol_id_to_span[symbol_id];
            //        Some(span.clone())
            //    }
            //    IdentType::Reference(reference_id) => {
            //        let reference = semantic.table.reference_id_to_reference.get(reference_id)?;
            //        let symbol_id = reference.symbol_id?;
            //        let symbol_range = semantic.table.symbol_id_to_span.get(symbol_id)?;
            //        Some(symbol_range.clone())
            //    }
            //};

            //range.and_then(|range| {
            //    let start_position = offset_to_position(range.start, &rope)?;
            //    let end_position = offset_to_position(range.end, &rope)?;
            //    Some(GotoDefinitionResponse::Scalar(Location::new(
            //        uri,
            //        Range::new(start_position, end_position),
            //    )))
            //})
        }();
        Ok(definition)
    }
    */

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
        let l = lexer::lex(params.text, 0);
        let mut p = parser::P::new(&l, &self.names);
        let _out = parser::module(&mut p);

        // let rope = ropey::Rope::from_str(params.text);
        // self.document_map
        //     .insert(params.uri.to_string(), rope.clone());
        


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
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

