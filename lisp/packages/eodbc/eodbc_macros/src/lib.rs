#![feature(trait_alias)]
use proc_macro::{self, TokenStream};
use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput, Data, visit::Visit, Field, Lit};

struct AttribVisitor {
    name: String,
    collected: Vec<(i16, Field)>
}

#[proc_macro_derive(FromRow, attributes(ord))]
pub fn derive_from_row(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let name = &input.ident;


    let data = match input.data {
        Data::Struct(dat) => dat,
        _ => panic!("Field must be struct")
    };

    let expanded = quote! {
        impl crate::traits::FromRow for #name {
            fn from_row(_: Vec<&[u8]>) -> std::option::Option<Self> {
                
                None
            }
        }
    };

    TokenStream::from(expanded)
}
