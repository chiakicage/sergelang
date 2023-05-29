enum List {
	Nil,
	Cons(i32, List)
}
fn __serge_user_main() -> i32 {
	let a: i32 = __serge_read_i32();
	let b: i32 = __serge_read_i32();
	let c: i32 = a + b;
	// let list: List = List::Cons(1, List::Cons(2, List::Nil));
	let e: f64 = 1.0;
	__serge_println(c);
	__serge_println(e);
	// __serge_println(list);
	0
}
