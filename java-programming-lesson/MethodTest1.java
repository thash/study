public class MethodTest1 {
    public static String[] getYourName() {
        String[] myoji_namae = { "yuuki", "hiroshi" };
        return myoji_namae;

    }
    public static void main(String[] args) {
        String[] s = getYourName();
        System.out.println("myoji is " + s[0]);
        System.out.println("namae is " + s[1]);
    }

}

