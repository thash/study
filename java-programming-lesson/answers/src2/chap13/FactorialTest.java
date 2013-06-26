public class FactorialTest {
    public static void main(String[] args) {
        System.out.println(factorial(10));
    }
    public static int factorial(int n) {
        return n * factorial(n - 1);
    }
}
