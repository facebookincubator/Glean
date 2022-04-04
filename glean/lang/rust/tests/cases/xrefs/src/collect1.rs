// ch 16: collections
#[allow(clippy::many_single_char_names)]
pub fn main() {
    // vectors
    {
         let mut ns: Vec<i32> = Vec::with_capacity(1);

         let words = vec!["step","on","no","pets"];

         let buf = [0u8; 1024];

         for i in words.iter().rev() {
             println!("{}",i);
         }
         assert_eq!(ns.get(0), None);

         let x = words[2];
         assert_eq!(x, "no");
         ns.push(3);

         let r = &buf[4..8];
         assert_eq!(Some(&r[0]), r.first());

         use std::collections::HashSet;

         // nub . sort
         let mut byte = b"Misssisssissssippppii".to_vec();
         let mut seen = HashSet::new();
         byte.retain(|r| seen.insert(*r)); // "Misp"
         println!("{}", String::from_utf8(byte).unwrap());

    }

    // concats and slices
    {
        assert_eq!([[1,2], [2,3]].concat(), [1,2,2,3]);
        println!("{}", [["foo","bar"], ["baz","qux"]].concat().join(", "));
    }

    // splitting
    {
        let i = 1;
        let j = 2;
        let mut v = vec![0, 1, 2, 3];
        let a = &mut v[i];
        *a += 2; // region of mutability ends
        let b = &v[j];
        let c = &v[i];

        println!("{:?}", c + b);
    }

}
