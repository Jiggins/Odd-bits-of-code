//His way
public static void fibonacci(int a, int b, int limit) {
	if (limit <= 0) {
		System.out.println()
	}
	else {
		int sum = a + b;
		limit = limit - 1;
		Sysytem.out.println(sum + " ");
		fibonacci(b, sum, limit); //That should be return fibonacci... but thats his notes.
	}
}

//My way
public static fibonacci(int n) {
	if (n == 0) return 0;
	if (n == 1) return 1;
	return fibonacci(n-1) + fibonacci(n-2))
}