pub fn main() {
    // ranges
    // ranges are 'half open'. they include the start value but not the end
    {
        // range full
        let a: std::ops::RangeFull = ..;
        let b: std::ops::RangeFull = std::ops::RangeFull;
        println!("{:?}", a);
        assert_eq!(a, b);

        // range full
        let a: std::ops::RangeFrom<bool> = true..;
        let b: std::ops::RangeFrom<bool> = std::ops::RangeFrom { start: true };
        println!("{:?}", a);
        assert_eq!(a, b);

        // interesting. this goes to infinity and will die with a overflow error
        // NOT like enumFrom
//         let b: std::ops::RangeFrom<u8> = 1..;
        let b: std::ops::Range<u8> = 1..255;
        for i in b {
            print!("{}, ", i);
        }
        println!("done");
    }
}
