// interesting. no tail recursion
// the obvious way:
#[inline]
pub fn sum_f1(n: u64) -> u64 {
    f1_go(n,0)
}

fn f1_go(n:u64, acc:u64) -> u64 {
    if n == 0 {
        acc
    } else {
        f1_go(n-1, acc + n)
    }
}

// explicit loop with mutable params
#[inline]
pub fn sum_f2(n: u64) -> u64 {
    let mut acc = 0;
    let mut m   = n;
    loop {
        if m == 0 {
            return acc;
        } else {
            acc += m;
            m -= 1;
        }
    }
}

// 
#[inline]
pub fn sum_f3(n: u64) -> u64 {
    let mut acc = 0;
    for m in 1 .. (n+1) {
        acc += m;
    }
    acc
}
