use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parser, parse_macro_input, DeriveInput, FnArg, GenericParam, ItemFn, Lifetime,
    LifetimeParam, PatIdent, PatType,
};

#[proc_macro_attribute]
pub fn element(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let struct_name = &ast.ident;

    // Check if the struct has generics
    let has_generics = !ast.generics.params.is_empty();

    // If there are no generics, add a lifetime (if necessary)
    if ast.generics.lifetimes().count() == 0 && !has_generics {
        ast.generics
            .params
            .push(syn::GenericParam::Lifetime(LifetimeParam::new(
                Lifetime::new("'a", proc_macro2::Span::call_site()),
            )));
    }

    // Modify the struct fields to include additional fields
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            if let syn::Fields::Named(fields) = &mut struct_data.fields {
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { pub children: Children })
                        .unwrap(),
                );
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { pub style: Style })
                        .unwrap(),
                );
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { pub id: &'a str })
                        .unwrap(),
                );
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { pub class: &'a str })
                        .unwrap(),
                );
            }
        }
        _ => panic!("`element` can only be used with structs"),
    }

    // Generate the impl block with generics if needed
    let impl_block = quote! {
        impl<'a> ElementCore for #struct_name<'a> {
            fn get_element_by_id(&mut self, id: &str) -> Option<&mut Element> {
                if let Children::Children(children, _) = &mut self.children {
                    for elem in children {
                        if elem.get_id() == id {
                            return Some(elem);
                        } else if let Some(e) = elem.get_element_by_id(id) {
                            return Some(e);
                        }
                    }
                }
                None
            }

            fn get_id(&self) -> String {
                self.id.to_string()
            }

            fn get_class(&self) -> String {
                self.class.to_string()
            }

            fn get_style(&self) -> &Style {
                &self.style
            }
        }
    };

    let expanded = quote! {
        #ast
        #impl_block
    };

    expanded.into()
}

#[proc_macro_attribute]
pub fn component(_: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);

    let fn_name = input_fn.sig.ident.clone();
    let code = input_fn.block;
    let visibility = input_fn.vis;
    let return_type = match input_fn.sig.output {
        syn::ReturnType::Default => syn::parse_quote! { Element },
        syn::ReturnType::Type(_, t) => t,
    };

    let lifetimes: Vec<_> = input_fn
        .sig
        .generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Lifetime(lifetime) = param {
                Some(&lifetime.lifetime)
            } else {
                None
            }
        })
        .collect();

    let mut struct_fields = Vec::new();

    for arg in input_fn.sig.inputs.iter() {
        if let FnArg::Typed(PatType { pat, ty, .. }) = arg {
            if let syn::Pat::Ident(PatIdent { ident, .. }) = &**pat {
                let field_name = ident.clone();
                let field_type = ty.clone();
                struct_fields.push(quote! { pub #field_name: #field_type });
            }
        }
    }

    let expanded = quote! {
        #[derive(Debug, Default)]
        #[allow(non_camel_case_types)]
        #visibility struct #fn_name<#(#lifetimes),*> {
            #(#struct_fields),*
        }

        impl <#(#lifetimes),*> Component for #fn_name <#(#lifetimes),*> {
            type Element = #return_type;
            fn create_element(self) -> Self::Element {
                #code
            }
        }
    };

    TokenStream::from(expanded)
}
