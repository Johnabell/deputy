use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Comma,
    Attribute, Expr, ExprArray, ExprPath, FnArg, Ident, ItemTrait, Meta, MetaNameValue, Path,
    ReturnType, TraitItem, TraitItemFn, Type,
};

pub struct MacroConfig {
    new_struct_name: Path,
    delegate_name: Path,
    result_types: Vec<Ident>,
    log_level: Ident,
}

impl MacroConfig {
    fn new(new_struct_name: Path, delegate_name: Path) -> Self {
        Self {
            new_struct_name,
            delegate_name,
            result_types: Self::default_result_types(),
            log_level: Self::default_log_level(),
        }
    }

    fn default_result_types() -> Vec<Ident> {
        vec![syn::parse2(quote!(Result)).unwrap()]
    }

    fn default_log_level() -> Ident {
        syn::parse2(quote!(error)).unwrap()
    }

    fn with_result_types(self, result_types: Vec<Ident>) -> Self {
        Self {
            result_types,
            ..self
        }
    }

    fn with_log_level(self, log_level: Ident) -> Self {
        Self { log_level, ..self }
    }
}

enum ParseError {
    UnexpectedType(Box<dyn ToTokens>, ExpectedType),
    UnknownOptionalConfig(Box<dyn ToTokens>, String),
}

impl From<ParseError> for syn::Error {
    fn from(value: ParseError) -> Self {
        match value {
            ParseError::UnexpectedType(expr, message) => Self::new_spanned(expr, message),
            ParseError::UnknownOptionalConfig(expr, message) => Self::new_spanned(expr, message),
        }
    }
}

enum ExpectedType {
    ResultTypesArray,
    NameValueList,
    ResultType,
    LogLevel,
}

impl std::fmt::Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unexpected type expected ")?;
        match self {
            ExpectedType::ResultTypesArray => write!(f, "array of result types such as `[MyResult, Result]`"),
            ExpectedType::NameValueList => write!(f, "a comma separated list of name value pairs such as `result_type = [MyResult, Result]`"),
            ExpectedType::ResultType => write!(f, "a type representing a result such as `MyResult`"),
            ExpectedType::LogLevel => write!(f, "a type representing a result such as `info`, or `error`"),
        }
    }
}

impl Parse for MacroConfig {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut details = Punctuated::<Meta, Comma>::parse_terminated(input)?.into_iter();
        let delegate_name = details.next().unwrap().require_path_only()?.clone();
        let new_struct_name = details.next().unwrap().require_path_only()?.clone();
        let mut config = Self::new(new_struct_name, delegate_name);
        for meta in details {
            config = match OptionalConfig::try_from(meta)? {
                OptionalConfig::RessultType(result_types) => config.with_result_types(result_types),
                OptionalConfig::LogLevel(level) => config.with_log_level(level),
            };
        }

        Ok(config)
    }
}

enum OptionalConfig {
    RessultType(Vec<Ident>),
    LogLevel(Ident),
}

impl OptionalConfig {
    const RESULT_TYPE: &str = "result_types";
    const LOG_LEVEL: &str = "log_level";
    const SUPPORTED_CONFIG_OPTIONS: [&str; 2] = [Self::RESULT_TYPE, Self::LOG_LEVEL];

    fn array_expr_to_idents(expr: Expr) -> Result<Vec<Ident>, ParseError> {
        match expr {
            Expr::Array(ExprArray { elems, .. }) => elems
                .into_iter()
                .map(|expr| Self::ensure_ident(expr, ExpectedType::ResultType))
                .collect(),
            _ => Err(ParseError::UnexpectedType(
                Box::new(expr),
                ExpectedType::ResultTypesArray,
            )),
        }
    }

    fn ensure_ident(expr: Expr, expected_type: ExpectedType) -> Result<Ident, ParseError> {
        match expr {
            Expr::Path(ExprPath { path, .. }) => path.last_segment_ident().ok_or(
                ParseError::UnexpectedType(Box::new(path), ExpectedType::ResultType),
            ),
            _ => Err(ParseError::UnexpectedType(Box::new(expr), expected_type)),
        }
    }
}

impl TryFrom<Meta> for OptionalConfig {
    type Error = ParseError;

    fn try_from(value: Meta) -> Result<Self, Self::Error> {
        match value {
            Meta::Path(path) => Err(ParseError::UnexpectedType(
                Box::new(path),
                ExpectedType::NameValueList,
            )),
            Meta::List(list) => Err(ParseError::UnexpectedType(
                Box::new(list),
                ExpectedType::NameValueList,
            )),
            Meta::NameValue(MetaNameValue { path, value, .. }) => match path.require_ident() {
                Err(_err) => Err(ParseError::UnexpectedType(
                    Box::new(value),
                    ExpectedType::NameValueList,
                )),
                Ok(ident) => {
                    let ident_name = ident.to_string();
                    match ident_name.as_str() {
                        OptionalConfig::RESULT_TYPE => {
                            let details = OptionalConfig::array_expr_to_idents(value)?;
                            Ok(OptionalConfig::RessultType(details))
                        }
                        OptionalConfig::LOG_LEVEL => {
                            let details = OptionalConfig::ensure_ident(value, ExpectedType::LogLevel)?;
                            Ok(OptionalConfig::LogLevel(details))
                        }
                        _ => Err(ParseError::UnknownOptionalConfig(
                            Box::new(path),
                            format!("Unknown configuration option {ident_name}, available options: {:?}", Self::SUPPORTED_CONFIG_OPTIONS),
                        )),
                    }
                }
            },
        }
    }
}

trait PathExt {
    fn last_segment_ident(&self) -> Option<Ident>;
}

impl PathExt for Path {
    fn last_segment_ident(&self) -> Option<Ident> {
        self.segments
            .iter()
            .last()
            .map(|segment| segment.ident.to_owned())
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

    fn impl_methods(&self, config: &MacroConfig) -> TokenStream {
        self.0
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Fn(method) => Some(Self::function_definition(method, config)),
                _ => None,
            })
            .collect()
    }

    pub fn to_token_stream(&self) -> TokenStream {
        self.0.to_token_stream()
    }

    fn function_definition(function: &TraitItemFn, config: &MacroConfig) -> TokenStream {
        let signature = function.sig.to_token_stream();
        let method_name = &function.sig.ident;
        let args: Punctuated<_, Comma> = function
            .sig
            .inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(&pat_type.pat),
                _ => None,
            })
            .collect();
        let maybe_async = function.sig.asyncness.map(|_| quote!(.await));
        let log_level = &config.log_level;

        if Self::is_fallible(&function.sig.output, &config.result_types) {
            quote!(
                #signature {
                    use tap::TapFallible;
                    self.0
                        .#method_name(#args)
                        #maybe_async
                        .tap_err(|error| {
                            tracing::#log_level!(?error, "Error calling {}", stringify!(#method_name))
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

    fn is_fallible(return_type: &ReturnType, result_types: &[Ident]) -> bool {
        match return_type {
            ReturnType::Type(_, ty) => {
                if let Type::Path(ref path) = **ty {
                    path.path
                        .last_segment_ident()
                        .map(|ident| result_types.contains(&ident))
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
            syn::Meta::Path(ref path) => path
                .last_segment_ident()
                .map(|ident| ident == "async_trait")
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
    let trait_name = trait_definition.name();
    let original_trait = trait_definition.to_token_stream();
    let impl_methods = trait_definition.impl_methods(&macro_config);
    let maybe_async_trait = trait_definition.maybe_async_trait();

    let new_struct_name = macro_config.new_struct_name;
    let delegate_name = macro_config.delegate_name;

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
            trait_definition
                .impl_methods(&MacroConfig::default())
                .to_string(),
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
    fn impl_methods_test_fallible_custom_level() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                fn method(&self, key: String) -> Result<(), &'static str>;
            }
        ))
        .unwrap();
        let config = MacroConfig::default().with_log_level(syn::parse2(quote!(info)).unwrap());

        assert_eq!(
            trait_definition.impl_methods(&config).to_string(),
            quote!(
                fn method(&self, key: String) -> Result<(), &'static str> {
                    use tap::TapFallible;
                    self.0.method(key).tap_err(|error| {
                        tracing::info!(?error, "Error calling {}", stringify!(method))
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
            trait_definition
                .impl_methods(&MacroConfig::default())
                .to_string(),
            quote!(
                fn method(&self, key: String) -> &'static str {
                    self.0.method(key)
                }
            )
            .to_string()
        );
    }

    #[test]
    fn impl_methods_test_fallible_async() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                async fn method(&self, key: String) -> Result<(), &'static str>;
            }
        ))
        .unwrap();

        assert_eq!(
            trait_definition
                .impl_methods(&MacroConfig::default())
                .to_string(),
            quote!(
                async fn method(&self, key: String) -> Result<(), &'static str> {
                    use tap::TapFallible;
                    self.0.method(key).await.tap_err(|error| {
                        tracing::error!(?error, "Error calling {}", stringify!(method))
                    })
                }
            )
            .to_string()
        );
    }

    #[test]
    fn impl_methods_test_infallible_async() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                async fn method(&self, key: String) -> &'static str;
            }
        ))
        .unwrap();

        assert_eq!(
            trait_definition
                .impl_methods(&MacroConfig::default())
                .to_string(),
            quote!(
                async fn method(&self, key: String) -> &'static str {
                    self.0.method(key).await
                }
            )
            .to_string()
        );
    }

    #[test]
    fn impl_methods_test_fallible_custom_result_types() {
        let trait_definition: TraitDefinition = syn::parse2(quote!(
            trait MyTrait {
                async fn method1(&self, key: String) -> MyResult<(), &'static str>;
                fn method2(&self, key: String) -> MyResult<(), &'static str>;
            }
        ))
        .unwrap();
        let config =
            MacroConfig::default().with_result_types(vec![syn::parse2(quote!(MyResult)).unwrap()]);

        assert_eq!(
            trait_definition.impl_methods(&config).to_string(),
            quote!(
                async fn method1(&self, key: String) -> MyResult<(), &'static str> {
                    use tap::TapFallible;
                    self.0.method1(key).await.tap_err(|error| {
                        tracing::error!(?error, "Error calling {}", stringify!(method1))
                    })
                }
                fn method2(&self, key: String) -> MyResult<(), &'static str> {
                    use tap::TapFallible;
                    self.0.method2(key).tap_err(|error| {
                        tracing::error!(?error, "Error calling {}", stringify!(method2))
                    })
                }
            )
            .to_string()
        );
    }

    impl Default for MacroConfig {
        fn default() -> Self {
            let delegate_name = syn::parse2(quote!(MyType)).unwrap();
            let new_type = syn::parse2(quote!(MyLoggingType)).unwrap();

            Self::new(new_type, delegate_name)
        }
    }
}
