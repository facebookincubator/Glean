// pg. 193: structs
// 
// types have CamelCase. fields and functions have snake case
//

#[derive(Clone)]
struct GSM { // greyscale map
    pixels: Vec<u8>,
    size: (usize, usize),
}

// tuple-like struct. product type with a newtype and indexing
#[derive(Debug)]
struct P(usize, usize, usize);

// hehey you can use them as newtypes
#[allow(dead_code)]
struct T(Vec<u8>);

// empty data types, wohoo
#[allow(dead_code)]
struct U;

fn mk_p(a: usize, b: usize, c: usize) -> P {
    P(a,b,c)
}

fn mk_gsm(w: usize, h: usize) -> GSM {
    GSM {
        pixels: vec![0; w*h],
        size: (w,h)
    }
}

pub fn main() {

    let w1 = 1024;
    let h1 = 576;

    let a1 = mk_gsm(w1,h1);
    let b1 = mk_gsm(w1/2, h1/2);

    let v1 = vec![0;0];
    let c1 = GSM { pixels: v1, .. b1 };

    // move pixels from 'a' , not copied!
    let d1 = GSM { pixels: a1.pixels.clone(), .. a1 };

    println!("{}", a1.pixels.len());
    println!("{}", b1.pixels.len());
    println!("{}", c1.pixels.len());
    println!("{}", d1.pixels.len());

    let a1 = mk_p(1,2,3);
    println!("{:?}", a1);
    println!("{:?}", a1.1);
}
