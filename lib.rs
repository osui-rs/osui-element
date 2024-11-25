use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parser, parse_macro_input, DeriveInput, Fields, GenericParam, Lifetime, LifetimeParam,
};

#[proc_macro_attribute]
pub fn element(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);

    // Add a lifetime if none exists
    if ast.generics.lifetimes().count() == 0 {
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

    let expanded = quote! {
        #ast
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
        pub fn #func_name<'a, T>() -> Box<#struct_name<'a, T>> {
            Box::new(#struct_name::default())
        }
    };

    let expanded = quote! {
        #ast
        #elem_fn
    };

    expanded.into()
}
