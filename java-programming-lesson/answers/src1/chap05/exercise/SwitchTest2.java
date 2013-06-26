public class SwitchTest2 {
    public static void main(String[] args) {
        int n = -1;

        switch (1 + n * n) {
        case 0:
            if (n > 0) {
                System.out.println("0A");
            } else {
                System.out.println("0B");
            }
            break;

        case 1:
            if (n > 0) {
                System.out.println("1C");
            } else {
                System.out.println("1D");
            }
            break;

        case 2:
            if (n > 0) {
                System.out.println("2E");
            } else {
                System.out.println("2F");
            }
            break;

        default:
            System.out.println("‚»‚êˆÈŠO");
            break;
        }
    }
}
