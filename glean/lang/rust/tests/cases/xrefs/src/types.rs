const V_I8: i8 = 0;
const V_I16: i16 = -5i16;
const V_I32: i32 = 2*8;
const V_I64: i64 = 20_922_789_888_000_000;

const V_U8: u8 = b'*';
const V_U16: u16 = 5u16;
const V_U32: u32 = 0;
const V_U64: u64 = 0;
#[allow(clippy::inconsistent_digit_grouping)]
const V_U128_1: u128 = 0b0011____0011_1101_0011_0011_11010011_0011_1101_0011_1101_0011_0011_11010011_0011_1101; 

const V_ISIZE: isize = 137;
const V_USIZE: usize = 0xffff_fc00usize;

#[allow(clippy::excessive_precision)]
const V_F32: f32 = 3.1415926536897832;
#[allow(clippy::excessive_precision)]
const V_F64: f64 = 3.141_592_653_689_783_2;

const V_TRUE: bool = true;
const V_FALSE: bool = false;

const V_CHAR: char = '\u{CA0}';

const V_UNIT: () = ();

const V_TUPLE: (char, u8, i32) = ('x', 42, 256);

#[derive(Debug)]
struct TyStruct {
    x: f32,
    y: f32,
} // type definiton

// named struct
const V_STRUCT: TyStruct = TyStruct { x: 32., y: 64. };

// tuple-struct ...
#[derive(Debug)]
struct TyTupStruct(i32, char);

const V_TUP_STRUCT: TyTupStruct = TyTupStruct(42, 'f');

// from https://doc.rust-lang.org/reference/items/constant-items.html
const BIT1: u32 = 1 << 3;
const BIT2: u32 = 1 << 1;

const BITS: [u32; 2] = [BIT1, BIT2];
const STRING: &str = "bitstring";

#[derive(Debug)]
struct BitsNStrings<'a> {
    mybits: [u32; 2],
    mystring: &'a str,
}

const BITS_N_STRINGS: BitsNStrings<'static> = BitsNStrings {
    mybits: BITS,
    mystring: STRING,
};

#[derive(Debug)]
struct Z; // woo hoo

const V_Z: Z = Z;

#[allow(dead_code)]
enum X {} // yeah boi

#[allow(dead_code)]
#[derive(Debug)]
enum Y {
    Z,
    X,
}

#[allow(dead_code)]
fn x() -> Y {
    Y::Z
}

// ok let's do it
#[derive(Debug)]
enum Expr {
    LAM(char, Box<Expr>),
    EVAL(Box<Expr>, Box<Expr>),
    CONST(u32),
    VAR(char),
    PRIM(char, Box<Expr>, Box<Expr>),
}

const E_EXP_ONE: Expr = Expr::CONST(1);
const E_EXP_TWO: Expr = Expr::CONST(2);
const E_EXP_VAR_X: Expr = Expr::VAR('x');

fn mk_exp_op(op: char, e1: Expr, e2: Expr) -> Expr {
    let box1 = Box::new(e1);
    let box2 = Box::new(e2);
    Expr::PRIM(op, box1, box2)
}

fn mk_exp_eval(e1: Expr, e2: Expr) -> Expr {
    let box1 = Box::new(e1);
    let box2 = Box::new(e2);
    Expr::EVAL(box1, box2)
}

fn mk_exp_lam(sym: char, e: Expr) -> Expr {
    let box1 = Box::new(e);
    Expr::LAM(sym, box1)
}

const V_REF: &i32 = &V_I32;

fn mk_suffix(x: char, s: &str) -> String {
    let mut t: String = (*s).to_string();
    t.push(x);
    t
}

const V_ARR_0: [u8; 4] = [0x00,0xe0,0x4c,0x68];

// what does this do?
fn mk_vec(vs: &[u8]) -> Vec<u8> {
    let us: Vec<u8> = vec!(vs[1]); // weird
    us
}

fn mk_3slice(vs: &[u8]) -> &[u8] {
    &vs[0..3] // 3 elements?

}

const V_FN_TY_0: fn() -> u32 = mk_u32_zero;
const V_FN_TY_1: fn() -> u32 = || 1;
const V_FN_TY_2: fn(u32,u32) -> u32 = |a,b| a*b;

fn mk_u32_zero() -> u32 {
    0
}

const V_U64_0: u64 = std::u64::MAX; // + 1;

const S_1: &str = "test 1 2 3";
const S_2: &str = "there \n\
                   once \
                   was";
const S_3: &str = r##"this
                  is a string
                  in complete free form.
         "##;

const S_4: [u8;3] = [b'A', b'B', b'C'];

pub fn main() {
    println!("i8  = {}", V_I8);
    println!("i16 = {}", V_I16);
    println!("i32 = {}", V_I32);
    println!("i64 = {}", V_I64);

    println!("u8  = {}", V_U8);
    println!("u16 = {}", V_U16);
    println!("u32 = {}", V_U32);
    println!("u64 = {}", V_U64);

    println!("isize = {}", V_ISIZE);
    println!("usize = {}", V_USIZE);

    println!("f32 = {}", V_F32);
    println!("f64 = {}", V_F64);

    println!("bool = {}", V_TRUE);
    println!("bool = {}", V_FALSE);

    println!("char = {} {}", V_CHAR, V_CHAR);

    println!("unit = {:?}", V_UNIT); // n.b. not Display

    println!("tuple = {:?}", V_TUPLE);

    println!("struct = {:?}", V_STRUCT);
    println!("struct2 = {:?}", BITS_N_STRINGS);
    println!("struct3 = {:?}", V_TUP_STRUCT);
    println!("struct4 = {:?}", V_Z);
    println!("struct5 = {:?}", x());

    println!("exp_1 = {:?}", E_EXP_ONE);
    println!("exp_2 = {:?}", E_EXP_TWO);
    println!("exp_3 = {:?}", mk_exp_op('*', E_EXP_ONE, E_EXP_TWO));
    println!("exp_3 = {:?}", mk_exp_eval(E_EXP_ONE, E_EXP_TWO));
    println!(
        "exp_4 = {:?}",
        mk_exp_lam('x', mk_exp_op('*', E_EXP_VAR_X, E_EXP_TWO))
    );

    println!("ref = {:?}", V_REF);
    let s = mk_suffix('#',"foo");
    println!("String = {:?}", s);
    println!("String = {:?}", &s[0..3]); // n.b runtime indexing errors

    println!("arr0 = {:?}", V_ARR_0);
    println!("arr1 = {:?}", mk_vec(&V_ARR_0));
    println!("arr2 = {:?}", mk_3slice(&V_ARR_0));

    println!("fn0 = {:?}", V_FN_TY_0);
    println!("fn1 = {:?}", V_FN_TY_0()); // app
    println!("fn2 = {:?}", V_FN_TY_1()); // app
    println!("fn2 = {:?}", V_FN_TY_2(4,6));

    println!("u64 = {:?}", V_U64_0);
    // println!("u64 = {:?}", V_U64_0 - V_U64_0 - 1);
    println!("u64 = {:?}", V_U64_0.wrapping_add(1));
    println!("u64 = {:?}", V_U64_0.wrapping_add(2));
    println!("u128 = {:?}", V_U128_1);

    assert_eq!( 10_i8 as u16, 10_u16);
    assert_eq!( 2525_u16 as i16, 2525_i16); 
    assert_eq!( -1_i16 as i32, -1_i32); // sign extension
    assert_eq!( 65535_u16 as i32, 65535_i32); // zero extended

    // bit truncation
    assert_eq!(1000_i16 as u8, 232_u8);
    assert_eq!(-1_i8 as u8, 255_u8);

    assert_eq!(2u16.pow(4), 16);
    assert_eq!(2_u16.pow(4), 16);
    assert_eq!(u16::pow(2,4), 16);

    println!("pi = {:?}", std::f64::consts::PI); // 3.141_592_653_589_793_f64);

    let c = std::char::from_u32(V_U32 + 123456);
    println!("char = {:?}", c);

    println!("string = {:?}", S_1);
    println!("string = {:?}", S_2);
    println!("string = {:?}", S_3);
    println!("string = {:?}", S_4);

}
