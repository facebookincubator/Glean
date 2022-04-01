/*
 * Pages 114 - 122
 * Sharing and mutation
 */
pub fn main() {
    // 1. sharing versus mutation
    {
        // v: ptr to contiguous block of i32 on heap, with capacity
        let v: Vec<i32> = vec![4, 8, 19, 27, 34, 10];
        {
            let r = &v; // borrow
            let x = r[0]; // read for its effect?
            println!("{}", x);
                  // drop r
        }
        let aside = v; // we can still use 'v', move v to aside
        let r = &aside;
        let x = r[0];
        println!("{}", x);
    }

    // 2. more sharing
    {
        fn extend_m(vec: &mut Vec<f64>, slice: &[f64]) {
            for e in slice {
                vec.push(*e);
            }
        }
        let mut a: Vec<f64> = vec![1.0, 2.0, 3.0];
        let b = [4.0, 5.0];
        extend_m(&mut a, &b);
        println!("{:?}", a);

        fn extend_pure(vec: &[f64], slice: &[f64]) -> Vec<f64> {
            let mut u = vec.to_owned();
            for e in slice {
                u.push(*e);
            }
            u // stack return
        }
        let a: Vec<f64> = vec![1.0, 2.0, 3.0];
        let b = [4.0, 5.0];
        let c = extend_pure(&a, &b);
        println!("{:?}", a);
        println!("{:?}", c);

        let mut wave = Vec::new();
        let head = vec![0.0, 1.0];
        let tail = [0.0, -1.0];
        extend_m(&mut wave, &head);
        extend_m(&mut wave, &tail);

        // this looks dubious. it might reallocate...
        // mutable and immutable lifetimes shouldn't overlap
        // extend_m(&mut wave, &wave);

        // this should be fine:
        let wave2 = extend_pure(&wave, &wave);
        println!("{:?}", wave2);
    }
    /*
     * shared references to values must be read-only in the lifetime of the shared ref.
     * mutable references mean those values are now exclusive to the lifetime of the reference
     */

    // 3. mutable borrowing and sharing examples, and ownership changes
    // n.b. similar example: borrow checker is shrinking the lifetimes from the book, so you need
    // to extend them to make it work
    {
        let /*mut*/ x1 = 10;
        let r1 = &x1;
        let r2 = &x1;    // shared ro borrow: ok
        // NO: x += 10;        // an effect. but it should be immutable now
        // NO: let m = &mut x; // borrow a mutable ref, should invalidate 'x' for lifetime of m
        assert_eq!(r1, r2); // extend lifetime to make it work

        let mut y1 = 20;
        let _m1 = &mut y1;
        // NO: let m2 = &mut y1; // a second mutable borrow. inconsistent
        // assert_eq!(m1, m2); // extend lifetime to make it work

        let /*mut*/ w1 = (107, 109); // mutable tuple
        let r1 = &w1;
        let _r0 = &r1.0; // OK. more shared ref
        // let m1 = &mut r.1; // borrow shared mut ref to 2nd component. nope

        let mut v1 = (136, 139);
        let m1 = &mut v1;
        let m0 = &mut m1.0; // reborrow mutable from mutable is ok?
        *m0 = 137; // destructive assignment to first field of tuple. to de-ref
        let _r1 = &m1.1;
        // let x = v.1; // NO: use of borrowed value, still in scope
        assert_eq!(m0,m0); // extend lifetime
        let y1 = v1.0; // copy . this is ok because shorter scoep of the mutables
        println!("{:?}",y1); // fine

    }
}
