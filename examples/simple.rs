enum List {
	Nil,
	Cons(i32, List)
}


fn __serge_user_main() {
	// let a: i32 = __serge_read_i32();
	// let b: i32 = __serge_read_i32();
	// let c: i32 = a + b;
	// // let list: List = List::Cons(1, List::Cons(2, List::Nil));
	// let e: f64 = 1.0;
	// __serge_println(c);
	// __serge_println(e);

	// if a > b {
	// 	__serge_println(666);
	// } else if a == b {
	// 	__serge_println(777);
	// } else {
	// 	__serge_println(888);
	// }
	// let i: i32 = 1;
	// while i < 10 {
	// 	__serge_println(i);
	// 	i = i + 1;
	// 	if i == 8 {
	// 		break;
	// 	}
	// }

	// for j in 20..30 {
	// 	// if j < 25 {
	// 		continue;
	// 	// }
	// 	// __serge_println(j);
	// }

	let a: [i32] = [1, 2, 3, 4, 5];
	__serge_println(a);
	a[0] = 10;
	__serge_println(a);
	// __serge_println(list);
	
}
fn test() {
	let a: i32 = 1;
}
