enum List {
    Nil,
    Cons(i32, List)
}

fn main() -> i32 {
	let tuple: (i32, i32) = (1, 2);
	// let list: List = List::Cons(1, List::Cons(2, List::Nil));
	println(tuple);
	0
}
