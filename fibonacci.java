public class fibonacci {
//His way
	public static void fibonacci(int a, int b, int limit) {
		if (limit <= 0) {
			System.out.println();
		}
		else {
			int sum = a + b;
			limit = limit - 1;
			System.out.println(sum + " ");
			fibonacci(b, sum, limit);
		}
	}

	//Phil Maguire
	/**
	*	Brief Description.
	*	@param n: the nth term in the fibonacci sequence.
	*	@return the nth fibonacci number.
	*/
	public static int fibonacci(int n) {
		if (n == 1) return 1;	//Early return statements, here we are giving the first two fibonacci numbers
		if (n == 2) return 2;	//so the program knows where to end.
		return fibonacci(n-1) + fibonacci(n-2);
	}

	public static int recursiveLoop(int n) {
		System.out.println(n);
		if (n == 0) {
			return 0;
		}

		return recursiveLoop(n-1);
	}

	public static int loopUp(int start, int limit) {
		System.out.println(start);
		if (start == limit) {
			return limit;
		}
		return loopUp(start+1, limit);
	}

	/**
	*	Taken from Sample Paper Section B.
	*/
	public static void pageRank() {
		double prx = 0.15;
		double pry = 0.15;
		double prz = 0.15;
		double d = 0.85;

		System.out.println("x:" + prx + " y: " + pry + " z: " + prz);

		for (int i = 0; i <= 30; i++) {
			prx = (1-d) + d * pry;
			pry = (1-d) + d * (prx + prz);
			prz = (1-d);
		}

		System.out.println("x:" + prx + " y: " + pry + " z: " + prz);
	}

	public static void main(String[] args) {
		System.out.println(fibonacci(7));
		pageRank();
	}
}