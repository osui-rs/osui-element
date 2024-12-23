use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, FnArg, ItemFn, Lifetime, PatIdent, PatType, Type, TypeReference};

#[proc_macro_attribute]
pub fn component(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);

    let fn_name = input_fn.sig.ident.clone();
    let code = input_fn.block;
    let visibility = input_fn.vis;
    let return_type = match input_fn.sig.output {
        syn::ReturnType::Default => syn::parse_quote! { Element },
        syn::ReturnType::Type(_, t) => t,
    };
    let mut fn_lifetime = false;

    let mut struct_fields = Vec::new();

    for arg in input_fn.sig.inputs.iter() {
        if let FnArg::Typed(PatType { pat, ty, .. }) = arg {
            if let syn::Pat::Ident(PatIdent { ident, .. }) = &**pat {
                let field_name = ident.clone();
                let mut field_type = *(ty.clone());

                if let Type::Reference(TypeReference { lifetime, .. }) = &mut field_type {
                    fn_lifetime = true;
                    *lifetime = Some(Lifetime::new("'a", Span::call_site()));
                }

                struct_fields.push(quote! { pub #field_name: #field_type });
            }
        }
    }

    let lifetime = if fn_lifetime {
        quote! {<'a>}
    } else {
        quote! {}
    };

    let expanded = quote! {
        #[derive(Debug, Default)]
        #[allow(non_camel_case_types)]
        #visibility struct #fn_name #lifetime {
            #(#struct_fields),*
        }

        impl #lifetime #fn_name #lifetime {
            fn create_element(self) -> #return_type {
                #code
            }
        }
    };

    TokenStream::from(expanded)
}
