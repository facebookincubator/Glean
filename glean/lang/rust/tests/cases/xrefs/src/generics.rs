// page 236: generics and traits

fn min<T: Ord>(a: T, b: T) -> T {
    if a < b {
        a
    } else {
        b
    }
}

pub fn main() {
    assert_eq!(min('a','b'), 'a');
    assert_eq!(min("foo","bar"),"bar");

    let vs = &vec![1,2,3,4,5,6,7];
    println!("{}", elem(vs, 5));
    println!("{}", elem(vs, 8));
    println!("{}", elem(vs, -1));
    println!("{}", elem(vs, 1));

    let vs = &vec!['a','c'];
    println!("{}", elem(vs, 'a'));
    println!("{}", elem(vs, 'b'));
    println!("{}", elem(vs, 'c'));

    ex1();
}

// write is a trait
use std::io::Write;

pub fn ex1() {
    let mut buf: Vec<u8> = vec![];

    // trait object.
    let _w: &mut dyn Write = &mut buf;
}

pub fn elem<P>(vec:&[P], x: P) -> bool
    where P : PartialEq
{
    for i in vec {
        if *i == x {
            return true
        }
    }
    false
}
