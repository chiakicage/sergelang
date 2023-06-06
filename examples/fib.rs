fn fib(n: i32) -> i32 {
	if n <= 1 {
		1
	} else {
		fib(n - 1) + fib(n - 2)
	}
}

fn gcd(a: i32, b: i32) -> i32 {
	if b == 0 {
		a
	} else {
		gcd(b, a % b)
	}
}

fn main() -> i32 {
	let n: i32 = read_i32();
	let f: i32 = fib(n);
	println(f);
	let a: i32 = read_i32();
	let b: i32 = read_i32();
	let g: i32 = gcd(a, b);
	println(g);
	0
}
