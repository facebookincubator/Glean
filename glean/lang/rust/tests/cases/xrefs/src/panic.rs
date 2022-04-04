// rustc -C panic=abbort panic.rs
#[allow(unreachable_code)]
pub fn main() {
    // unwinding
    {
        println!("{}", pirate_share(100, 15));
        println!("{}", pirate_share(100, 1));
    }

    panic!("{:?}", panic!()); // explicit panic.

    // interesting:
    fix();

    {
        println!("{}", pirate_share(100, 0)); // panicked at 'attempt to divide by zero'
    }
}

fn fix() -> ! {
    loop {panic!()}
}

fn pirate_share(total: u64, crew_size: usize) -> u64 {
    (total / 2) / crew_size as u64
}
