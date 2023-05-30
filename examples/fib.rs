// fn gcd(a: i32, b: i32) -> i32 {
// 	if b == 0 {
// 		a
// 	} else {
// 		gcd(b, a % b)
// 	}
// }

fn fib(n: i32) -> i32 {
	if n <= 1 {
		1
	} else {
		fib(n - 1) + fib(n - 2)
	}
}

fn __serge_user_main() -> i32 {
	let a: i32 = __serge_read_i32();
	__serge_println(a);
	for i in 0..a {
	let v: i32 = fib(i);
	__serge_println(v);
	}
	0
}
