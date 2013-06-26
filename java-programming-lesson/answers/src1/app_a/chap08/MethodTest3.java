public class MethodTest3 {
    public static void getYourName(String[] args) {
        args[0] = "Œ‹é";
        args[1] = "_";
    }
    public static void main(String[] args) {
        String[] s = new String[2];
        getYourName(s);
        System.out.println("–¼š‚Í" + s[0]);
        System.out.println("–¼‘O‚Í" + s[1]);
    }
}
