pub fn main() {

    println!("** exp.rs **");

    let x: &str = match Some(("foo", 7)) {
        None => "none",
        Some((y @ "bar", _)) => y,
        Some(("foo", 7)) => "foo_7",
        _ => "anything else",
    };
    println!("{}", x);

    // 1. declarations
    {
        let name: i64 = if 1 > 2 { 7 } else { 8 };
        let name2 = if 1 > 2 { 7 } else { 8 };
        assert_eq!(name, name2);
    }

    // 2 case analysis
    {
        // interesting: blocks evaluate to ()
        #[allow(clippy::if_same_then_else)]
        let _v = if true {
        } else if 7 > 8 {
        } else {
        };
    }

    // 3. matching
    {
        let _v = match (42, 3) {
            (0, 6) => "7",
            (1, 4) => "8",
            (2, 5) => "9",
            (3, 6) => "10",
            (4, 7) => "11",
            // non-exhaustive patterns, nice!
            _ => "sure",
        };
    }

    // 4.0 enums
    {
        #[allow(dead_code)]
        enum Message {
            Quit,
            Move { x: i32, y: i32 },
            Write(String),
            ChangeColor(i32, i32, i32),
        }
        let msg = Message::Write("my custom message".to_string());

        let v: String = match msg {
            Message::Quit => "The Quit variant has no data to destructure.".to_string(),
            Message::Move { x, y } => {
                format!("Move in the x direction {} and in the y direction {}", x, y)
            }
            Message::Write(text) => text,
            Message::ChangeColor(r, g, b) => {
                format!("Change the color to red {}, green {}, and blue {}", r, g, b)
            }
        };
        println!("{}", v);
    }

    // 4. matching on sum types
    {
        // generic ADT-like
        enum ETy<T> {
            A { f: T, sz: u32 },
            B { f: T, sz: u32, _a: u64 },
            C { f: T, sz: u32, _count: u32 },
        }

        let v1 = ETy::A {
            f: "my_v1".to_string(),
            sz: 7,
        };
        let v2 = ETy::B {
            f: "my_v2".to_string(),
            sz: 8,
            _a: 7,
        };
        let v3 = ETy::C {
            f: "my_v3".to_string(),
            sz: 7,
            _count: 0,
        };

        let res = match (v2, v1, v3) {
            (ETy::A { f, sz, .. }, _, _) => format!("first is A : {} @ {}", f, sz),
            (ETy::B { f, sz, .. }, _, _) => format!("first is B : {} @ {}", f, sz),
            (ETy::C { f, sz, .. }, _, _) => format!("first is C : {} @ {}", f, sz),
            // unreachable!! _ => "fall through".to_string(),
        };

        println!("{:?}", res);
    }

    // 5. if let
    {
        let v = Some("foo");

        #[allow(clippy::redundant_pattern_matching)]
        if let None = v {
            println!("WAT");
        } else {
            println!("OK");
        };
    }

    // 6. loops
    {
        let mut n: u64 = 1;
        while n < 100 {
            n *= 2;
        }
        println!("{}", n);

        let mut n: Option<u64> = Some(1);
        while let Some(x) = n {
            if x < 100 {
                n = Some(x * 2);
            } else {
                break;
            }
        }
        println!("{:?}", n);

        let mut i: u64 = 1;
        loop {
            if i >= 100 {
                break;
            }
            i *= 2;
        }
        println!("{:?}", i);

        // for is a 'map' over a collection. nice.
        let mut n: u64 = 0;
        for i in 0..20 {
            // n.b. while < 20!!!
            n += i;
        }
        println!("{:?}", n);

        let mut v = vec![];
        for i in 0..20 {
            // 20 elements. last element has value '19'
            v.push(i);
        }
        println!("{:?}", v);
        println!("{:?}", v.len());

        let strs: Vec<String> = vec!["a".to_string(), "b".to_string()];
        for s in &strs {
            println!("{}", s);
        }
        println!("{}", strs.len());

        let mut strs: Vec<String> = vec!["a".to_string(), "b".to_string()];
        for s in &mut strs {
            s.push('#');
        }
        println!("{:?}", strs);

        // interesting. loop lifetimes
        let bs = [1, 2, 3];
        #[allow(clippy::never_loop)]
        'foo: for _ in &bs {
            break 'foo;
        }

        // return expressions (!)
        {
            fn f0() {
                // return; // value of ().
            }
            #[allow(unreachable_code)]
            #[allow(clippy::diverging_sub_expression)]
            fn f1() -> usize {
                let _v = return 2 * 128; // weird
                0 /* dead code */
            }
            println!("{:?}", f0());
            println!("{:?}", f1());

            #[allow(unreachable_code)]
            #[allow(clippy::diverging_sub_expression)]
            fn f2<'a>() -> &'a [u64] {
                let _a: &[u64] = &[
                    1,
                    2,
                    return &[FOO], //f2 evaluates to this
                ];
                println!("{:?}", _a);
            }
            println!("{:?}", f2());
        }
    }

    // whoa. flow sensitive typing
    #[allow(unreachable_code)]
    {
        // break and return don't produce a value
        let mut n: u64 = 1;
        let a: () = while n < 10 {
            n *= 2
        };
        println!("{:?}", a);
        println!("about to fix:");

        // this produces a value of any type
        // this type is that it can be cast to any other one
        // a bottom value
        #[allow(dead_code)]
        fn fix() -> ! {
            loop {
                //break; // can't break if it diverges
                panic!();
            }
        }
        // let _v: Option<u64> = fix();

        // let _x: &[i32] = &[1, 2, undefined()];

        // fix();

        println!("fixed");
    }

    // some syntax stuff
    {
        let mut x = /* Vec<i32>::with_capacity(1024); */
                    Vec::<i32>::with_capacity(1024);
        println!("{:?}", x.pop());
    }

}

// can't have a 'return' outside of fn body :
const FOO: u64 = /*return*/ 0xdeadbeef;

#[allow(dead_code)]
fn undefined() -> ! {
    loop {panic!()}
}
