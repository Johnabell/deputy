use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Comma,
    Attribute, FnArg, Ident, ItemTrait, Path, ReturnType, TraitItem, TraitItemFn, Type,
};

pub struct MacroConfig {
    pub new_struct_name: Ident,
    pub delegate_name: Ident,
}

impl Parse for MacroConfig {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        // TODO should I use Meta here?
        let mut details = Punctuated::<Ident, Comma>::parse_terminated(input)?.into_iter();
        let trait_name = details.next().unwrap();
        let new_struct_name = details.next().unwrap();

        Ok(Self {
            new_struct_name,
            delegate_name: trait_name,
        })
    }
}

pub struct TraitDefinition(ItemTrait);

impl Parse for TraitDefinition {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self(ItemTrait::parse(input)?))
    }
}

impl TraitDefinition {
    pub fn name(&self) -> &Ident {
        &self.0.ident
    }

    fn impl_methods(&self) -> TokenStream {
        self.0
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Fn(method) => Some(Self::function_definition(method)),
                _ => None,
            })
            .collect()
    }

    pub fn to_token_stream(&self) -> TokenStream {
        self.0.to_token_stream()
    }

    fn function_definition(function: &TraitItemFn) -> TokenStream {
        let signature = function.sig.to_token_stream();
        let method_name = &function.sig.ident;
        // TODO ensure it takes self
        let args: Punctuated<_, Comma> = function
            .sig
            .inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(&pat_type.pat),
                _ => None,
            })
            .collect();
        // TODO ignore associated functions
        let maybe_async = function.sig.asyncness.map(|_| quote!(.await));

        if Self::is_fallible(&function.sig.output) {
            quote!(
                #signature {
                    use tap::TapFallible;
                    self.0
                        .#method_name(#args)
                        #maybe_async
                        .tap_err(|error| {
                            tracing::error!(?error, "Error calling {}", stringify!(#method_name))
                        })
                }
            )
        } else {
            quote!(
                #signature {
                    self.0
                        .#method_name(#args)
                        #maybe_async
                }
            )
        }
    }

    fn is_fallible(return_type: &ReturnType) -> bool {
        match return_type {
            ReturnType::Type(_, ty) => {
                if let Type::Path(ref path) = **ty {
                    path.path
                        .segments
                        .iter()
                        .last()
                        .map(|segment| segment.ident == "Result")
                        .unwrap_or(false)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn is_async_trait_attr(attribute: &Attribute) -> bool {
        match attribute.meta {
            syn::Meta::Path(Path { ref segments, .. }) => segments
                .iter()
                .last()
                .map(|segment| segment.ident == "async_trait")
                .unwrap_or(false),
            _ => false,
        }
    }

    fn is_async_trait(&self) -> bool {
        self.0.attrs.iter().any(Self::is_async_trait_attr)
    }

    fn maybe_async_trait(&self) -> Option<TokenStream> {
        if self.is_async_trait() {
            Some(quote!(
               #[async_trait::async_trait]
            ))
        } else {
            None
        }
    }
}

pub fn deputy(
    macro_config: MacroConfig,
    trait_definition: TraitDefinition,
) -> Result<TokenStream, &'static str> {
    let new_struct_name = macro_config.new_struct_name;
    let delegate_name = macro_config.delegate_name;
    let trait_name = trait_definition.name();
    let original_trait = trait_definition.to_token_stream();
    let impl_methods = trait_definition.impl_methods();
    let maybe_async_trait = trait_definition.maybe_async_trait();

    Ok(quote! {
        #original_trait

        struct #new_struct_name(#delegate_name);

        impl From<#delegate_name> for #new_struct_name {
            fn from(value: #delegate_name) -> Self {
                Self(value)
            }
        }

        #maybe_async_trait
        impl #trait_name for #new_struct_name {
            #impl_methods
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn is_async_trait_test() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            #[async_trait]
            trait MyTrait {}
        ))
        .unwrap();

        assert!(trait_definition.is_async_trait());

        let trait_definition: TraitDefinition = syn::parse2(quote!(
            #[async_trait::async_trait]
            trait MyTrait {}
        ))
        .unwrap();

        assert!(trait_definition.is_async_trait());
    }

    #[test]
    fn trait_definition_parse_test() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {}
        ))
        .unwrap();

        assert_eq!(trait_definition.name(), "MyTrait");
    }

    #[test]
    fn impl_methods_test_fallible() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                fn method(&self, key: String) -> Result<(), &'static str>;
            }
        ))
        .unwrap();

        assert_eq!(
            trait_definition.impl_methods().to_string(),
            quote!(
                fn method(&self, key: String) -> Result<(), &'static str> {
                    use tap::TapFallible;
                    self.0.method(key).tap_err(|error| {
                        tracing::error!(?error, "Error calling {}", stringify!(method))
                    })
                }
            )
            .to_string()
        );
    }

    #[test]
    fn impl_methods_test_infallible() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                fn method(&self, key: String) -> &'static str;
            }
        ))
        .unwrap();

        assert_eq!(
            trait_definition.impl_methods().to_string(),
            quote!(
                fn method(&self, key: String) -> &'static str {
                    self.0.method(key)
                }
            )
            .to_string()
        );
    }
}
