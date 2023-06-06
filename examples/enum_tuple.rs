enum List {
    Nil,
    Cons(i32, List),
}

fn print_list(list: List) {
	match list {
		List::Nil => {},
		List::Cons(x, xs) => {
			println(x);
			print_list(xs);
		}
	}
}

fn main() -> i32 {
	let tuple: (i32, i32) = (1, 2);
	let list: List = List::Cons(5, List::Cons(6, List::Nil));
	println(tuple);
	match tuple {
		(x, y) => {
			println(x);
			println(y);
		}
	}
	print_list(list);
	0
}
