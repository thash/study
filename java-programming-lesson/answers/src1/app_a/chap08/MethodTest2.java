public class MethodTest2 {
    public static String[] getYourName() {
        String[] myoji_namae = new String[2];
        myoji_namae[0] = "Œ‹é";
        myoji_namae[1] = "_";
        return myoji_namae;
    }
    public static void main(String[] args) {
        String[] s = getYourName();
        System.out.println("–¼š‚Í" + s[0]);
        System.out.println("–¼‘O‚Í" + s[1]);
    }
}
