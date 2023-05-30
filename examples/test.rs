fn add(a: i32, b: i32) -> i32 {
    let c : i32 = a + b;
    c
}
fn sub(a: i32, b: i32) -> i32 {
    let c: i32 = a - b;
    c
}

fn main() {
    let a : i32 = 1;
    let b : i32 = 1;

    while a < 200 {
        let c : i32 = a;
        a = add(b, c);
        b = c;
    }
    
}
