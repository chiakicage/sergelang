fn fib(n: i32) -> i32 {
	if n <= 1 {
		1
	} else {
		fib(n - 1) + fib(n - 2)
	}
}

fn main() -> i32 {
	let n: i32 = read_i32();
	let f: i32 = fib(n);
	println(f);
	0
}
