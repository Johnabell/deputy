#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use deputy::deputy;
trait MyTrait {
    fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str>;
}
struct LoggingMyType(MyType);
impl From<MyType> for LoggingMyType {
    fn from(value: MyType) -> Self {
        Self(value)
    }
}
use tap::TapFallible;
impl MyTrait for LoggingMyType {
    fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str> {
        self.0.fallible(arg1, arg2).tap_err(|error| {
            use ::tracing::__macro_support::Callsite as _;
            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                static META: ::tracing::Metadata<'static> = {
                    ::tracing_core::metadata::Metadata::new(
                        "event deputy/tests/error_loging.rs:3",
                        "error_loging",
                        ::tracing::Level::ERROR,
                        ::core::option::Option::Some("deputy/tests/error_loging.rs"),
                        ::core::option::Option::Some(3u32),
                        ::core::option::Option::Some("error_loging"),
                        ::tracing_core::field::FieldSet::new(
                            &["message", "error"],
                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                        ),
                        ::tracing::metadata::Kind::EVENT,
                    )
                };
                ::tracing::callsite::DefaultCallsite::new(&META)
            };
            let enabled = ::tracing::Level::ERROR <= ::tracing::level_filters::STATIC_MAX_LEVEL
                && ::tracing::Level::ERROR <= ::tracing::level_filters::LevelFilter::current()
                && {
                    let interest = __CALLSITE.interest();
                    !interest.is_never()
                        && ::tracing::__macro_support::__is_enabled(__CALLSITE.metadata(), interest)
                };
            if enabled {
                (|value_set: ::tracing::field::ValueSet| {
                    let meta = __CALLSITE.metadata();
                    ::tracing::Event::dispatch(meta, &value_set);
                })({
                    #[allow(unused_imports)]
                    use ::tracing::field::{debug, display, Value};
                    let mut iter = __CALLSITE.metadata().fields().iter();
                    __CALLSITE.metadata().fields().value_set(&[
                        (
                            &::core::iter::Iterator::next(&mut iter)
                                .expect("FieldSet corrupted (this is a bug)"),
                            ::core::option::Option::Some(&() as &dyn Value),
                        ),
                        (
                            &::core::iter::Iterator::next(&mut iter)
                                .expect("FieldSet corrupted (this is a bug)"),
                            ::core::option::Option::Some(&debug(&error) as &dyn Value),
                        ),
                    ])
                });
            } else {
            }
        })
    }
}
struct MyType(bool);
impl MyTrait for MyType {
    fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str> {
        if self.0 {
            Ok(arg1)
        } else {
            Err(arg2)
        }
    }
}
extern crate test;
#[cfg(test)]
#[rustc_test_marker = "can_call_method"]
pub const can_call_method: test::TestDescAndFn = test::TestDescAndFn {
    desc: test::TestDesc {
        name: test::StaticTestName("can_call_method"),
        ignore: false,
        ignore_message: ::core::option::Option::None,
        source_file: "deputy/tests/error_loging.rs",
        start_line: 21usize,
        start_col: 4usize,
        end_line: 21usize,
        end_col: 19usize,
        compile_fail: false,
        no_run: false,
        should_panic: test::ShouldPanic::No,
        test_type: test::TestType::IntegrationTest,
    },
    testfn: test::StaticTestFn(|| test::assert_test_result(can_call_method())),
};
fn can_call_method() {
    let logging_type = LoggingMyType::from(MyType(true));
    let result = logging_type.fallible(String::from("Ok"), "Error");
    match (&result, &Ok(String::from("Ok"))) {
        (left_val, right_val) => {
            if !(*left_val == *right_val) {
                let kind = ::core::panicking::AssertKind::Eq;
                ::core::panicking::assert_failed(
                    kind,
                    &*left_val,
                    &*right_val,
                    ::core::option::Option::None,
                );
            }
        }
    };
    let logging_type = LoggingMyType::from(MyType(false));
    let result = logging_type.fallible(String::from("Ok"), "Error");
    match (&result, &Err("Error")) {
        (left_val, right_val) => {
            if !(*left_val == *right_val) {
                let kind = ::core::panicking::AssertKind::Eq;
                ::core::panicking::assert_failed(
                    kind,
                    &*left_val,
                    &*right_val,
                    ::core::option::Option::None,
                );
            }
        }
    };
}
#[rustc_main]
#[coverage(off)]
pub fn main() -> () {
    extern crate test;
    test::test_main_static(&[&can_call_method])
}
