#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_derive(Ast)]
pub fn ast_derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as syn::DeriveInput);

    let ident = input.ident;
    let generics = input.generics;
    let inner = match input.data {
        syn::Data::Struct(s) => {
            let fields = match s.fields {
                syn::Fields::Named(syn::FieldsNamed { named: fs, .. })
                | syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed: fs, .. }) => fs
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        f.ident
                            .as_ref()
                            .map(|x| quote!(self.#x.show(indent__ + 1, w__)?;))
                            .unwrap_or_else(|| {
                                let i = syn::Index::from(i);
                                quote!(self.#i.show(indent__ + 1, w__)?;)
                            })
                    })
                    .collect(),
                syn::Fields::Unit => Vec::new(),
            };
            quote!(
                writeln!(w__, "{:indent__$}{}", "", stringify!(#ident), indent__ = indent__)?;
                #(#fields)*
            )
        }

        syn::Data::Enum(s) => {
            let match_arms: Vec<_> = s
                .variants
                .iter()
                .map(|v| {
                    let vident = v.ident.clone();
                    let (args, body) = match &v.fields {
                        syn::Fields::Named(syn::FieldsNamed { named: fs, .. })
                        | syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed: fs, .. }) => fs
                            .iter()
                            .enumerate()
                            .map(|(i, f)| {
                                let n = f.ident.as_ref().map(|x| quote!(#x)).unwrap_or_else(|| {
                                    let i = syn::Ident::new(
                                        &format!("a{}", i),
                                        proc_macro2::Span::call_site()
                                    );
                                    quote!(#i)
                                });
                                (quote!(#n , ), quote!(#n.show(indent__ + 1, w__)?;))
                            })
                            .collect::<(Vec<_>, Vec<_>)>(),

                        syn::Fields::Unit => (Vec::new(), Vec::new()),
                    };
                    let args2 = match v.fields {
                        syn::Fields::Named(_) => quote!( { #(#args)* } ),
                        syn::Fields::Unnamed(_) => quote! { ( #(#args)* ) },
                        syn::Fields::Unit => quote!(),
                    };
                    quote! {
                        #ident::#vident #args2 => {
                            writeln!(w__, "{:indent__$}{}::{}", "", stringify!(#ident), stringify!(#vident), indent__ = indent__)?;
                            #(#body)*
                        },
                    }
                })
                .collect();

            quote!(
                match self {
                    #(#match_arms)*
                };
            )
        }
        syn::Data::Union(_) => {
            panic!("Unions aren't supported yet")
        }
    };

    let out = TokenStream::from(quote!(
        impl #generics Ast for #ident #generics {
            fn show(&self, indent__: usize, w__: &mut impl ::std::io::Write) -> ::std::io::Result<()> {
                #inner
                Ok(())
            }
        }
    ));
    // println!("Output: {}", out);
    out
}

struct ParserHelperInput {
    name: syn::Ident,
    ty: syn::Type,
    vars: Vec<(bool, syn::Expr)>,
    out: syn::Expr,
}

impl syn::parse::Parse for ParserHelperInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        input.parse::<Token![->]>()?;
        let ty: syn::Type = input.parse()?;
        input.parse::<Token![|]>()?;

        let mut vars = Vec::new();
        while !input.is_empty() {
            let is = input.parse::<Option<Token![_]>>().err();
            let f: syn::Expr = input.parse()?;
            vars.push((is, f));
        }

        input.parse::<Token![=>]>()?;
        let out: syn::Expr = input.parse()?;

        Ok(Input {
            name,
            ty,
            vars,
            out,
        })
    }
}

#[proc_macro]
pub fn prs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ParserHelperInput);

    let name = input.name;
    let ty = input.ty;
    let vars = input.vars;
    let out = input.out;

    let code = vars
        .iter()
        .enumerate()
        .map(|(i, (is, f))|
            if *is {
                let i = syn::Ident::new(&format!("v{}", i), proc_macro2::Span::call_site());
                quote!{
                    let #i = #f (p) ? ;
                }
            } else {
                quote!{
                    #f (p) ? ;
                }
            }
        ).collect();

    let done = vars
        .iter()
        .enumerate()
        .map(|(i, (is, _))|
            if *is {
                let i = syn::Ident::new(&format!("v{}", i), proc_macro2::Span::call_site());
                quote!{ #i , }
            } else {
                quote!{}
            }
        ).collect();

    // Generate code based on the parsed input
    let expanded = quote! {
        pub fn #name<'t>(p: &mut P<'t>) -> #ty {
            #(#code)
            #(#done)
        }
    };

    TokenStream::from(expanded)
}
