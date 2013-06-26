public class MethodTest2 {
    public static String[] getYourName() {
        String[] myoji_namae = new String[2];
        myoji_namae[0] = "yuuki";
        myoji_namae[1] = "hiroshi";
        return myoji_namae;
    }
    public static void main(String[] args) {
        String[] s = getYourName();
        System.out.println("myoji is " + s[0]);
        System.out.println("namae is " + s[1]);
    }

}

