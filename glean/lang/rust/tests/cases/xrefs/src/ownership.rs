pub fn main() {
    print_padovan();
    let s = boxing();
    println!("{}", s);
    let t = move_thing();
    println!("{:?}", t);
    let t = move_thing1();
    println!("{:?}", t);
    let t = move_thing2();
    println!("{}",t);
    move_thing3();
    move_thing4();
    let t = move_thing5();
    println!("{:?}",t);
}

fn print_padovan() {
    let a = vec![1, 1, 1]; // local var on stack, ptr to heap
    let mut b = a; // drops a
    for i in 3..10 {
        let next = b[i - 3] + b[i - 2];
        b.push(next);
    }
    println!("P(1..10) = {:?}", b); // drop b and reclaim the heap
}

fn boxing() -> String {
    let point = Box::new((0.625, 0.5)); // ptr to heap alloc
    let label = format!("{:?}", point); // ptr to heap alloc str
    label
}

fn move_thing() -> (Vec<String>, Vec<String>) {
    let s = vec!["a".to_string(), "b".to_string()];
    let t = s.clone();
    // let u = s; // move
    let u = s.clone();
    println!("{:?}", s);
    (t,u)
}

fn move_thing1() -> String {
    #[allow(unused_assignments)]
    let mut s = "Govinda".to_string();
    s = "Siddhartha".to_string(); // re-assign drops 
    s
}

fn move_thing2() -> String {
    #[derive(Debug)]
    struct Person {
        name: String,
        birth: i32,
    }

    let mut c = Vec::new(); // new empty vector
    c.push(Person {
        name: "Palestrina".to_string(),
        birth: 1525,
    });

    format!("{:?}", c)
}

fn move_thing3() {
    let c = true;
    let x = vec![10, 20, 30];
    if c {
        f(x); // owns x
    } else {
        g(x); // owns x
    }
    // h(x); // either path uses it

    fn f(_: Vec<i32>) {}
    fn g(_: Vec<i32>) {}
   // fn h() -> bool {true}

   //  let x = vec![10, 20, 30];
   //  while h() {
   //      g(x); // loop moves x
   //  }

    let mut v = Vec::new();
    for i in 101 .. 106 {
        v.push(i.to_string());
    }
    // let third = v[2]; // move fragment of v
    let _fifth = &v[4]; // a ref is ok
}

fn move_thing4() {
    let mut v = Vec::new();
    for i in 101i8 .. 106 {
        v.push(i.to_string());
    }

    // pop value off the end
    let fifth = v.pop().unwrap();
    assert_eq!(fifth, "105");

    // remove a value out
    let second = v.swap_remove(1);
    assert_eq!(second, "102");

    let third = std::mem::replace(&mut v[2], "substitute".to_string());
    assert_eq!(third, "103"); // the value that is removed

    println!("{:?}", v);

    let v = vec!["a".to_string(), "b".to_string(), "c".to_string()];

    for mut s in v {
        s.push('!'); // needs to be mut
        println!("{}", s);
    }
}
#[derive(Debug)]
struct Person {
    name: Option<String>,
    birth: i32,
}

fn move_thing5() -> (Option<String>, Vec<Person>) {
    let mut c = Vec::new();
    c.push(Person {
        name: Some("Palestrina".to_string()),
        birth: 1525,
    });

    // let n = c[0].name; // move component
    let n = c[0].name.take(); // swaps for None

    (n, c)
}
