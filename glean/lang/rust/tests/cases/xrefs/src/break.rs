pub fn break_main() {
    // 1. can't borrow a reference to a local variable and take it out of scope
    {
        let r: &i64 = &1; // empty ref binding
                          // assert_eq!(*r, 1); // uniitialized *r.
        {
            let _x = 2;
            // NO: r = &x; // borrowed value doesn't live long enough
            // ref to x can't outlive x
        } // dropped here
        assert_eq!(*r, 1);
    }

    // 2. a valid example
    {
        let x = 1;
        {
            let r = &x; // inner life time is subset of lifetime of x
            assert_eq!(*r, 1);
        }
    }

    // 3. borrwing a ref to a data structure
    {
        let v = vec![1, 2, 3];
        {
            let r = &v[1];
            assert_eq!(*r, 2);
        }
    }

    // 4. references in parameters
    {
        // threadsafety. this is a top-level IORef.
        // need to treat like an MVar
        // mutable static is pretty unsafe
        static mut STASH: &i32 = &128; // needs to be initialized
                                       // f can only applied to things with static lifetime
        fn f(p: &'static i32) {
            // tick A
            // ignore the threadsafety explicitly
            unsafe {
                STASH = p; // but lifetime of p
            }
        }
        f(&17);
        // lifetime genericity , most specific lifetime etc
        // not ok: let WORTH_POINTING_AT: i32 = 1000;
        static WORTH_POINTING_AT: i32 = 1000;
        // also ok: const WORTH_POINTING_AT: i32 = 1000;
        f(&WORTH_POINTING_AT);
    }

    // 5. passing references as arguments
    {
        // polymorphic in lifetime
        #[allow(clippy::needless_lifetimes)]
        fn g<'a>(_p: &'a i32) {}
        fn h(_p: &'static i32) {}

        let x = 10;
        g(&x);
        const UY: i32 = 10;
        h(&UY);
    }

    // 6. slices
    {
        // with a single ref in and return a single ref, assume same lifetime
        // fn smallest(v: &[i32]) -> &i32 {
        #[allow(clippy::needless_lifetimes)]
        fn smallest<'a>(v: &'a [i32]) -> &'a i32 {
            let mut s = &v[0];
            for r in &v[1..] {
                // iterate by ref
                if *r < *s {
                    s = r;
                }
            }
            s
        }

        {
            let parabola = [9, 4, 1, 0, 1, 4, 9];
            let s = smallest(&parabola);
            assert_eq!(*s, 0);
        }
    }

    // 7. structs with references
    // you must pass a lifetime param through the struct to any references contained
    {
        #[derive(Debug,PartialEq,Clone)]
        struct S<'a> {
            r: &'a i32,
        }

        let x = 10;
        let s = S { r: &x }; // some lifetime 'a, which must be within x's lifetime
        assert_eq!(*s.r, 10);

        // and they're contagious
        struct T<'a> {
            s: S<'a>
        }

        let u = s.clone();
        let t = T { s }; // move 's'

        // can't do this: assert_eq!(t.s, s);
        assert_eq!(t.s, u);
    }

    // 8. lifetime params: more
    // this works (the book says it shouldn't because lifetime of x and y are different)
    {
        struct U<'a,'b> {
            x: &'a i32,
            y: &'b i32
        }

        let a = 10;
        let r1;
        let r2;
        {
            let y = 20;
            {
                let s = U { x: &a, y: &y };
                r1 = s.x;
                assert_eq!(*r1, 10);
                r2 = s.y;
                assert_eq!(*r2, 20);
            }
        }
        assert_eq!(*r1, 10); // this extends the lifetime until it works
        /*
         * [Know PR]
         * Page 111 bottom code example
         *
         * The reader is correct: the book says that code doesn't compile, but it does with recent
         * versions of Rust. This is because the borrow checker has been improved: despite its
         * scope `r` is never actually used beyond the lifetime of `y`, so the borrow checker
         * concludes that the lifetime of the two references in `S` does not actually extend too
         * far to be safe.
         *
         * To be fixed in the second edition
         */

    }

    // 9. omitting parameters
    {
        struct S<'a, 'b> {
            x: &'a i32,
            y: &'b i32
        }
        let a = S { x: &10 , y: &20};

        // interesting type conversion through references
        #[allow(clippy::needless_lifetimes)]
        fn sum_r_xy<'a, 'b, 'c>(r: &'a i32, s: S<'b, 'c>) -> i32 {
            r + s.x + s.y
        }

        assert_eq!(sum_r_xy(&8, a), 38);

        // single lifetime: so you can elide them
        fn first_third(point: &[i32; 3]) -> [i32; 2] {
            [point[0], point[2]]
        }

        let v: [i32; 3] = [1, 2, 3];
        let w: [i32; 2] = [1,    3];
        assert_eq!(first_third(&v), w);

    }

    // 10. methods
    {
        struct T {
            els: Vec<String>,
        }
        impl T {
            fn findl_by_prefix(&self, prefix: &str) -> Option<&String> {
                for e in &self.els {
                    if e.starts_with(prefix) {
                        return Some(&e);  // a ref.
                    }
                }
                None
            }
        }

        let ks = vec!["aardvark".to_string(),"aaple".to_string(), "boo".to_string()];
        let t = T { els: ks };
        match t.findl_by_prefix(&"aa") {
            None => { println!("None found"); }
            Some(xs) => println!("{:?}", xs)
        };

    }
}
