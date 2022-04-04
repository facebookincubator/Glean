use std::str::FromStr;
use std::panic as std_panic;

mod bintree;
mod bounds;
mod cast;
mod closures;
mod collect;
mod collect1;
mod compare;
mod copytypes;
mod r#enum;
mod r#enum2;
mod expr2;
mod expr;
mod foo;
mod generics;
mod r#impl;
mod r#impl2;
mod io;
mod iter;
mod r#json;
mod lib;
mod mod2;
mod modules;
mod mutable;
mod operators;
mod ownership;
mod panic;
mod pattern;
mod r#break;
mod r#f64;
mod rc;
mod result;
mod sharing;
mod r#struct;
mod r#trait;
mod types;
mod vecdeque;
mod unicode;

fn main() {
    let mut ns: Vec<u64> = Vec::new();

    for arg in std::env::args().skip(1) {
        let i = u64::from_str(&arg);
        let r = i.expect("Parser error on numeric argument");
        ns.push(r);
    }

    if ns.is_empty() {
        eprintln!("Usage: gcd NUMBER ...");
        std::process::exit(1);
    }

    // foldl1 over args
    let mut r1 = ns[0]; // there's at least one argument
    for p in &ns[1..] {
        r1 = gcd(r1, *p);
    }

    println!("The GCD of {:?} is {}", ns, r1);

    let s1 = str_games("I see the eigenvalue in thine eye");
    println!("{}", s1);

    let b1 = array_stuff_2();
    for _ in 2 .. 10 {
        println!("{:?}", b1[0]);
    }

    let (v1,u1) = vec_stuff();
    for i in &v1 {
        println!("{:?}", *i);
    }
    let f = |x: Vec<i32>| x.iter().product::<i32>(); // fold(1, |a,b| a*b);
    println!("PRODUCT = {}", f(v1));
    println!("PRODUCT = {}", f(u1));

    let s: Vec<&str> = vec!["a man", "a plan", "a canal", "panama"];
    let t = vec_reverse_functional(s); // moves s to vec_stuff
    // can't use s now
    for l in t {
        println!("str := {}", l);
    }

    // foo module
    let f = foo::new_foo(43);
    println!("Foo: {:?}", f);

    // break module
    r#break::break_main();

    bintree::main();
    bounds::main();
    cast::cast_main();
    closures::main();
    collect::main();
    collect1::main();
    compare::main();
    copytypes::main();
    r#enum::main();
    r#enum2::main();
    expr::main();
    expr2::main();
    generics::main();
    r#f64::main();
    r#impl::main();
    r#impl2::main();
    io::main();
    iter::main();
    r#json::main();
    modules::main();
    mutable::main();
    mod2::main();

    println!("{}", lib::sum_f1(1000));
    println!("{}", lib::sum_f2(1000));
    println!("{}", lib::sum_f3(1000));

    operators::main();
    ownership::main();

    {
        let r = std_panic::catch_unwind(|| {
            panic::main(); // done
        });
        assert!(r.is_err());
    }
    pattern::main();

    rc::main();
    result::main();
    sharing::main();
    r#struct::main();
    r#trait::main();
    types::main();
    vecdeque::main();
    unicode::main();

    println!("All done, thankyou.");
}

fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!(n != 0 && m != 0);
    while m != 0 {
        if m < n {
            std::mem::swap(&mut m, &mut n);
        }
        m %= n;
    }
    n
}

#[test]
fn test_gcd() {
    assert_eq!(gcd(14, 15), 1);
    assert_eq!(gcd(2 * 3 * 5 * 11 * 17, 3 * 77 * 11 * 13 * 19), 3 * 11);
}

#[allow(dead_code)]
// allocate a new vector and poke some values in
fn build_vector() -> Vec<i16> {
    let mut v = Vec::new();
    v.push(10i16);
    v.push(20);
    v
}

fn str_games(t: &str) -> String {
    let (x, _xs) = str::split_at(t,21);
    let t = (12, "eggs"); // stack allocated. unboxed
    let _b = Box::new(t);
    x.to_string()
}

#[test]
fn array_stuff() {
    let lazy_caterer: [u32; 6] = [1, 2, 4, 7, 11, 16];
    let taxonomy = ["Animalia", "Anthropoda", "Insecta"];

    assert_eq!(lazy_caterer[3], 7);
    assert_eq!(taxonomy.len(), 3);
}



fn array_stuff_2() -> Box<[bool]> {
    const A_LIM: usize = 10000;
    let mut sieve = [true; A_LIM];
    for i in 2..100 {
        if sieve[i] {
            let mut j = i * i;
            while j < A_LIM {
                sieve[j] = false;
                j += i;
            }
        }
    }
    assert!(sieve[211]);
    assert!(!sieve[9876]);

    Box::new(sieve)
}

fn vec_stuff() -> (Vec<i32>, Vec<i32>) {
    let v: Vec<i32> = vec![2, 3, 5, 7]; // via a macro

    let u: Vec<i32> = (1..6).collect();

    assert_eq!(v.iter().product::<i32>(), 210); //fold(1, |a, b| a * b), 210);
    (u,v)
}

fn vec_reverse_functional(mut s: Vec<&str>) -> Vec<&str> {
    s.reverse();
    s // returns the borrowed value
}

#[allow(dead_code)]
fn str_stuff() {
    let noodles: String = "noodles".to_string(); // copied, construct a boxed String
    let _oodles: &str = &noodles[1..]; // a slice
    let _poodles: &str = "abc";
    let _zoodles = &noodles[0..2];
}

