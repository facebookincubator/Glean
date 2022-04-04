// a module
// public type
#[derive(Debug)]
pub struct Foo {
    deep_foo : u64,
}

// public interface
pub fn new_foo(deep_foo: u64) -> Foo {
    new_secret_foo(deep_foo)
}

// not exported from the module
fn new_secret_foo(deep_foo: u64) -> Foo {
    Foo { deep_foo }
}

#[test]
fn main() {
    let foo = new_foo(42);
    println!("Foo: {:?}", foo);
}
