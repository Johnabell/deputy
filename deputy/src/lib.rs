use deputy_core::{MacroConfig, TraitDefinition};
use proc_macro2::TokenStream;
use syn::parse_macro_input;

extern crate proc_macro;

#[proc_macro_attribute]
pub fn deputy(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let macro_config = parse_macro_input!(attr as MacroConfig);
    let trait_definition = parse_macro_input!(item as TraitDefinition);

    let output: TokenStream =
        deputy_core::deputy(macro_config, trait_definition).unwrap_or_else(|e| panic!("{}", e));

    output.into()
}
