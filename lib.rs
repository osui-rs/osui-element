use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parser, parse_macro_input, DeriveInput, Fields, GenericParam, Lifetime, LifetimeParam,
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
            .push(GenericParam::Lifetime(LifetimeParam::new(Lifetime::new(
                "'a",
                proc_macro2::Span::call_site(),
            ))));
    }

    // Modify the struct fields to include additional fields
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            if let Fields::Named(fields) = &mut struct_data.fields {
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
pub fn elem_fn(_args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name = &ast.ident;

    let func_name = syn::Ident::new(
        &struct_name.to_string().to_lowercase(),
        proc_macro2::Span::call_site(),
    );

    let elem_fn = quote! {
        pub fn #func_name<'a>() -> Box<#struct_name<'a>> {
            Box::new(#struct_name::default())
        }
    };

    let expanded = quote! {
        #ast
        #elem_fn
    };

    expanded.into()
}
