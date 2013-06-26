public class SwitchTest1 {
    public static void main(String[] args) {
        int n = 1;

        switch (2 * n + 1) {
        case 0:
            System.out.println("ゼロ");
            break;

        case 1:
            System.out.println("イチ");
            break;

        case 2:
            System.out.println("ニ");
            break;

        case 3:
            System.out.println("サン");
            break;

        default:
            System.out.println("それ以外");
            break;
        }
    }
}
