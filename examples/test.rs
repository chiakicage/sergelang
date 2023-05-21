fn gcd(a: int, b: int) -> int {
	if b == 0 { a } else { gcd(b, a % b) }
}

enum Name {
	First(int),
	Last {
		first: int,
		last: (int, int),
	}
}

fn add(a: int, b: int) -> int {
	a + b
}
fn sub(a: int, b: int) -> int {
	a - b
}

fn main() -> int {
	let z: float = 1.5;
	let a: int = 1;
	let b: int = 2;
	let c: (int, int, int) = (1, 2, 3);
	let d: [int] = [1, 2, 3];
	let f: fn(int, int) -> int = sub;
	let g: fn(int, int) -> int = |a: int, b: int| -> int { a + b };
	let t: ((int, int), int) = ((1, 2), 3);
	let e: [int] = d;
	let c: int = add(a, b);
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
	let first: int = 1;
	let last: (int, int) = (2, 3);
	let f: Name = Name::Last { first, last: last };
	match f {
		Name::First(a) => a,
		Name::Last { first, last: (a, b) } => a + b + first,
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

