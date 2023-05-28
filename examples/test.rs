fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

enum Name {
    First(i32),
    Last { first: i32, last: (i32, i32) },
}

enum List {
    Nil,
    One(i32),
    Cons(i32, List)
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn sub(a: i32, b: i32) -> i32 {
    a - b
}

fn main() -> i32 {
    let z: f64 = 1.5;
    let a: i32 = 1;
    let b: i32 = 2;
    let c: (i32, i32, i32) = (1, 2, 3);
    let d: [i32] = [1, 2, 3];
    let f: fn(i32, i32) -> i32 = sub;
    let g: fn(i32, i32) -> i32 = |a: i32, b: i32| -> i32 { a + b };
    let t: ((i32, i32), i32) = ((1, 2), 3);
    let e: [i32] = d;
    let c: i32 = add(a, b);
    if a > b {
        a
    } else if a < b {
        b
    } else if a == b {
        c
    } else {
        a
    }
    if a < b {
        a
    }
    let d: Name = Name::First(123);
    let first: i32 = 1;
    let last: (i32, i32) = (2, 3);
    let f: Name = Name::Last { first, last: last };
    match f {
        Name::First(a) => a,
        Name::Last {
            first,
            last: (1, b),
        } => a + b + first,
        Name::Last {
            first,
            last: (a, b),
        } => c + b + first,
    }
    let list: (List, List) = (List::Nil, List::Nil);
    // not exhaustive
    // match list {
    //     (_, List::Nil) => 1,
    //     (List::Nil, _) => 2,
    // }
    match list {
        (_, List::Nil) => 1,
        (List::Nil, _) => 2,
        (List::One(_), _) => 3,
        (_, List::One(_)) => 4,
        (List::Cons(_, _), _) => 5,
        (_, List::Cons(_, _)) => 6,
    }
    for i in 0..10 {
        first = i;
    }
    for i in 0..10 {
        first = i;
        break;
    }
    for i in 0..10 {
        first = i;
        continue;
    }
    for i in 0..10 {
        first = g(i, i + 1);
        if i == 5 {
            break;
        }
    }
    for i in 0..10 {
        first = i;
        if i == 5 {
            continue;
        }
    }
    while a >= b {
        a = a - 1;
    }
    return a;
    0
}
