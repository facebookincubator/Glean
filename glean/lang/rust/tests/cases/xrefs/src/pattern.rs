// Pg 223. Patterns

pub fn main() {

    // literals
    {
        let x = (100,"foo");
        if let (100, "foo") = x {};
    }

    // ranges
    {
        let x = 42;
        println!("{}" , match x {
            // warning: `...` range patterns are deprecated!
            // 40 ... 43 => "yes",
            40 ..= 43 => "yes",
            _ => "no"
        });
    }

    // variables. borrow a mutable variable to the pattern match
    {
        let x = 42;

        println!("{}" , match x {
            // 40 ... 43 => "yes",
            mut y@40 ..= 43 => { y*=2 ; format!("{}",y) },
            _ => "no".to_string()
        });

        println!("{}" , match x {
            ref y@40 ..= 43 => { format!("{}",*y) },
            _ => "no".to_string()
        });

        println!("{}", x);
    }

    // guards and structures
    {
        struct Color(u64,u64,u32);
        let v = Color(256,3,52);
        println!("{}", match v {
            Color(r,_,_) if r > 200 => r,
            _ => 0
        });
    }

    // how far does the exhaustiveness checking go?
    {
        fn foo(n: u64) -> u64 {
            let mut i = n;
            loop {
                if i == 0 {
                    return n;
                } else {
                    i += 2;
                }
            }
        }

        let v = 0 /* diverges: 1 */;
        println!("{}", match v {
            #[allow(clippy::eq_op)]
            _ if true == true => "wat",
            _ if foo(v) == 0 => "yes",
            _ => "sigh",
        });

    }

}
