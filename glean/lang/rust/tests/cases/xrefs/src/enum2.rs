// Chapter 10: Enums. page 212

/* trees */
#[derive(Debug, Clone, PartialEq)]
enum Tree<T> {
    Empty,
    /* tag */
    Full(Box<Node<T>>),
    /* tag , ptr */
}

#[derive(Debug, Clone, PartialEq)]
struct Node<T> {
    element: T,
    left: Tree<T>,
    right: Tree<T>,
}

use Tree::*;

fn empty<T>() -> Tree<T> {
    Empty
}

// box a singleton tree
fn single<T>(e: T) -> Tree<T> {
    let n = Box::new(Node {
        element: e,
        left: Empty,
        right: Empty,
    });

    Full(n)
}

// for all T in Ord, Tree<T> has impl ...
impl<T: Ord> Tree<T> {

    // mutable insertion with pattern matching
    fn add_mut(&mut self, v: T) {
        match *self {
            Empty => {
                *self = single(v);
            },
            Full(ref mut n) => {
                if v <= n.element {
                    Tree::add_mut(&mut n.left, v);
                } else {
                    Tree::add_mut(&mut n.right, v);
                }
            }
        }
    }
}

// functional insertion
#[allow(clippy::many_single_char_names)]
fn add<T: Ord>(t: Tree<T>, v: T) -> Tree<T> {
    match t {
        Empty => {
            single(v)
        },
        Full(n) => {
            let Node { element: e, left: l, right: r  } = *n;
            if v <= e {
                Full(Box::new(Node {
                    element: e,
                    left : add(l,v),
                    right: r
                }))

            } else {
                Full(Box::new(Node {
                    element: e,
                    left : l,
                    right: add(r,v)
                }))
           }
        }
    }
}

// both functional and mutable trees
pub fn main() {
    let mut t1 = empty();
    t1.add_mut(7);
    t1.add_mut(8);
    t1.add_mut(3);
    println!("{:?}", t1);

    let t2 = empty();
    let t3 = add(t2, 7);
    let t4 = add(t3, 8);
    let t5 = add(t4, 3);
    println!("{:?}", t5);

    assert_eq!(t1, t5);

}
