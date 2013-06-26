public class MethodTest1 {
    public static String[] getYourName() {
        String[] myoji_namae = { "Œ‹é", "_" };
        return myoji_namae;
    }
    public static void main(String[] args) {
        String[] s = getYourName();
        System.out.println("–¼š‚Í" + s[0]);
        System.out.println("–¼‘O‚Í" + s[1]);
    }
}
