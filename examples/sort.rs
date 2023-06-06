fn main() -> i32 {
	let a: [i32] = [2, 1, 4, 7, 4, 8, 3, 6, 4, 7];
	let n: i32 = 10;
	println(a);
	println(n);
	for i in 0..n {
		for j in (i + 1)..n {
			if a[i] > a[j] {
				let t: i32 = a[i];
				a[i] = a[j];
				a[j] = t;
			}
		}
	}
	println(a);
	println(len(a));
	let sum: i32 = 0;
	for i in 0..len(a) {
		if i % 2 == 0 {
			continue;
		} 
		sum = sum + a[i];
	}
	println(sum);
	0
}
