// binary trees as iterators
#[derive(Copy,Clone)]
enum Tree<'a, T> {
    Empty,
    NonEmpty(Node<'a, T>)
}

#[derive(Copy,Clone)]
struct Node<'a,T> {
    e: T,
    left: &'a Tree<'a,T>,
    right: &'a Tree<'a,T>
}

#[allow(clippy::many_single_char_names)]
pub fn main() {
    let a = make_node(&Empty, 1, &Empty);
    assert_eq!(count(&a), 1);
    let sub = a; // copy
    let b = make_node(&sub, 2, &Empty);
    assert_eq!(count(&b), 2);
    let sub1 = make_node(&Empty, 7 , &Empty);
    let c = make_node(&b, 3, &sub1);
    assert_eq!(count(&c), 4);

    // iterate over a binary tree
    let mut v = Vec::new();
    for i in &c {
        v.push(*i);
    }
    println!("{:?}", v);

    let d = c.iter().map(|e| e * 2).collect::<Vec<i64>>();
    println!("{:?}", d);
}

fn count<T>(t: &Tree<T>) -> usize {
    match t {
        Tree::Empty => 0,
        Tree::NonEmpty(n) => count(&n.left) + count(&n.right) + 1
    }
}

use self::Tree::*;

struct TreeIter<'a, T> {
    // maintain our own stack to iterate
    unvisited: Vec<&'a Node<'a, T>>
}

// define a method on TreeIter
impl<'a, T: 'a> TreeIter<'a, T> {
    fn push_left_edge(&mut self, mut tree: &'a Tree<T>) {
        while let NonEmpty(ref node) = *tree {
            self.unvisited.push(node);
            tree = &node.left;
        }
    }
}

// "give" an iter method to Tree
impl<T> Tree<'_, T> {
    fn iter(&self) -> TreeIter<T> {
        let mut iter = TreeIter { unvisited: Vec::new() };
        iter.push_left_edge(self);
        iter
    }
}

impl<'a, T: 'a> IntoIterator for &'a Tree<'a, T> {
    type Item = &'a T;
    type IntoIter = TreeIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

// a flattening transformation
impl<'a, T> Iterator for TreeIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        let node = match self.unvisited.pop() {
            None => return None, // short circuit
            Some(n) => n
        };
        self.push_left_edge(&node.right);
        Some(&node.e)
    }
}

fn make_node<'a, T>(left: &'a Tree<'a, T>, e: T, right: &'a Tree<'a, T>) -> Tree<'a, T> {
    NonEmpty(Node{e, left, right})
}
