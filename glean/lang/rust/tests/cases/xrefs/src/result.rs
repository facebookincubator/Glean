// Result is like all sorts of Maybe/Optional monad stuff
//
use std::io;

// Error Handling: Result
pub fn main() {
    let arr: &[u8] = &[6, 1, 2, 0, 1, 2, 3, 4, 5, 6];

    println!("{:?}", find(&|x| { x / 3 == 2 }, arr));
    println!("{:?}", find(&|x| { x == 9 }, arr));
    println!("{:?}", find(&|x| { x == 6 }, arr));

    let s = match get_weather() {
        Ok(v) => format!("bool : {}", v),
        Err(e) => format!("err : {}", e),
    };
    println!("{}", s);

    if get_weather().is_ok() {
        println!("It's ok!");
    }

    if get_weather().is_err() {
        println!("It is an error!");
    }

    println!(
        "ok? {}",
        match get_weather().ok() {
            Some(true) => "true enough",
            _ => "not really true",
        }
    );

    println!(
        "err? {}",
        match get_weather().err() {
            Some(e) => format!("{:?}", e),
            _ => "not really an error".to_string(),
        }
    );

    println!("all good: {}", get_weather().unwrap_or(false));
    println!(
        "all good: {}",
        get_weather().expect("well that's not success")
    );

    if let Err(e) = wrapping() {
        println!("{:?}", e);
        std::process::exit(1);
    }

    match get_weather2() {
        Ok(_) => (), // std::process::exit(42),
        Err(MyError { problem }) => println!("{}", problem),
    };
}

fn wrapping() -> Result<(), io::Error> {
    // unwrap
    let _v = get_weather()?;

    // equiv to
    let _v = match get_weather() {
        Ok(v) => v,
        Err(e) => return Err(e),
    };

    Ok(())
}

fn find(f: &dyn Fn(u8) -> bool, p: &[u8]) -> Option<u8> {
    match find_index(f, p) {
        Some(n) => Some(p[n]),
        _ => None,
    }
}

fn find_index(k: &dyn Fn(u8) -> bool, p: &[u8]) -> Option<usize> {
    // for n in 0..p.len() {
    for (n, i) in p.iter().enumerate() {
        if k(*i) {
            return Some(n);
        }
    }
    None
}

/*

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing
{-# INLINE find #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex k (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go !ptr !n | n >= l    = return Nothing
               | otherwise = do w <- peek ptr
                                if k w
                                  then return (Just n)
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndex #-}

*/

type MyResult<T> = Result<T, io::Error>;
type MyBoolResult = MyResult<bool>;

fn get_weather() -> MyBoolResult {
    Ok(true)
}

fn get_weather2() -> Result<bool, MyError> {
    if 1 > 2 {
        Err(MyError {
            problem: "This cannot be".to_string(),
        })
    } else {
        Ok(true)
    }
}

/* implement a custom error type */
#[derive(Debug, Clone)]
pub struct MyError {
    pub problem: String,
}
