// ch12 operator overloading

#[derive(Clone,Copy,Debug)]
struct Complex<T> {
    re: T,
    im: T
}

// unary operators
use std::ops::{Neg,Add};

impl<T> Neg for Complex<T> where T: Neg<Output=T> {
    type Output = Complex<T>;
    fn neg(self) -> Self::Output {
        Complex { re: -self.re, im: -self.im }
    }
}

impl<L, R, O> Add<Complex<R>> for Complex<L>
    where L: Add<R, Output=O>
{
    type Output = Complex<O>;
    fn add(self, rhs: Complex<R>) -> Self::Output {
        Complex { re: self.re + rhs.re
                , im: self.im + rhs.im }
    }
}

// addition with mutable assignment?
use std::ops::AddAssign;

impl<T> AddAssign for Complex<T> where T: AddAssign<T>
{
    // x += y;
    fn add_assign(&mut self, rhs: Complex<T>) {
        self.re += rhs.re;
        self.im += rhs.im;
    }

}

// equality
impl<T: PartialEq> PartialEq for Complex<T> {
    fn eq(&self, other: &Complex<T>) -> bool {
        self.re == other.re && self.im == other.im
    }
    /* use default ne */
}

/* test */
pub fn main() {
    let x = Complex { re: 4, im: 2};
    let y = -x;
    let z = -y;

    let mut o = Complex { re: 4, im: 16 };
    o += z;

    println!("{:?}", y);
    println!("{:?}", z);
    println!("{:?}", z + z);
    println!("{:?}", o);

}
