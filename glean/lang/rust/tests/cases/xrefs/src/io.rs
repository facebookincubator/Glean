// ch18: IO . page 432.
//
use std::fs::File;

pub fn main() {
    let h = File::open("io.rs").unwrap();
    let m = h.metadata().unwrap();
    println!("{:?}", m);

    assert_eq!(false, m.is_dir());

    let mut one = File::open("io.rs").unwrap();
    let mut two = File::create("/tmp/io.rs").unwrap();
    copy(&mut one, &mut two).unwrap();

    // higher level io
    {
        let mut one = File::open("io.rs").unwrap();
        let mut v = Vec::new();
        one.read_to_end(&mut v).unwrap();
        println!("{}", v.len());

        let mut one = File::open("io.rs").unwrap();
        let mut s = String::new();
        one.read_to_string(&mut s).unwrap();
        println!("{}", &s[0..22]);
    }
}

use std::io::{self, Read, Write, ErrorKind};

const DEFAULT_BUF_SIZE: usize = 8 * 1024;

// generic copy of buffers
pub fn copy<R: ?Sized, W: ?Sized>(reader: &mut R, writer: &mut W)
    -> io::Result<u64>
    where R: Read, W: Write
{
    let mut buf = [0; DEFAULT_BUF_SIZE];
    let mut count = 0;

    loop {
        let len = match reader.read(&mut buf) {
            Ok(0) => return Ok(count),
            Ok(len) => len,
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        };
        writer.write_all(&buf[..len])?;
        count += len as u64;
    }
}
