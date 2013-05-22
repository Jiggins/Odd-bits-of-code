//His way
public static void fibonacci(int a, int b, int limit) {
	if (limit <= 0) {
		System.out.println()
	}
	else {
		int sum = a + b;
		limit = limit - 1;
		Sysytem.out.println(sum + " ");
		fibonacci(b, sum, limit);	//That should be return fibonacci... but thats from his notes.
	}
}

//My way
public static fibonacci(int n) {
	if (n == 0) return 0;	//Early return statements, here we are giving the first two fibonacci numbers
	if (n == 1) return 1;	//so the program knows where to end.
	return fibonacci(n-1) + fibonacci(n-2));
}