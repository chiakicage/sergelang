fn add(a: i32, b: i32) -> i32 {
    let c : i32 = a + b;
    c
}
fn sub(a: i32, b: i32) -> i32 {
    let c: i32 = a - b;
    c
}

fn main() {
    let z: f64 = 1.5;
    let a: i32 = 1;
    let b: i32 = 2;
    
    let c: f64 = z - add(a, b) + sub(a, b);
    let c1: i32 = a * b;
    let c2: f64 = b * z;
    let d: bool = (a != b);
    let e: bool = (a == b);
    let f: bool = (a > b);
    let g: bool = (a < b);
    let h: bool = (a >= b);
    let i: bool = (a <= b);
    let res : i32 = (2 * (a + b) * (a - b) / 2);

    for t  in 0..10 {
        for r in 0..20 {
            if a > b {

            }
            else if a < b {

            }
            else if a < b {

            }
            else {
                
            }
        }
    }
    
}
