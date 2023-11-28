use async_trait::async_trait;
use deputy::deputy;

#[deputy(MyType, LoggingMyType)]
trait MyTrait {
    fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str>;
    fn infallible(&self, arg1: String) -> String;
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
    fn infallible(&self, arg1: String) -> String {
        arg1
    }
}

#[deputy(MyAsyncType, LoggingMyAsyncType)]
#[async_trait]
trait MyAsyncTrait {
    async fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str>;
    async fn infallible(&self, arg1: String) -> String;
}

struct MyAsyncType(bool);

#[async_trait]
impl MyAsyncTrait for MyAsyncType {
    async fn fallible(&self, arg1: String, arg2: &'static str) -> Result<String, &'static str> {
        if self.0 {
            Ok(arg1)
        } else {
            Err(arg2)
        }
    }
    async fn infallible(&self, arg1: String) -> String {
        arg1
    }
}

#[test]
fn can_call_method() {
    let logging_type = LoggingMyType::from(MyType(true));
    let result = logging_type.fallible(String::from("Ok"), "Error");

    assert_eq!(result, Ok(String::from("Ok")));
    assert_eq!(
        logging_type.infallible(String::from("Hello")),
        String::from("Hello")
    );

    let logging_type = LoggingMyType::from(MyType(false));
    let result = logging_type.fallible(String::from("Ok"), "Error");

    assert_eq!(result, Err("Error"));
}
