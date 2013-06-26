public class MethodTest3 {
    public static void getYourName(String[] args) {
        args[0] = "yuuki";
        args[1] = "hiroshi";
    }
    public static void main(String[] args) {
        String[] s = new String[2];
        getYourName(s);
        System.out.println("myoji is " + s[0]);
        System.out.println("namae is " + s[1]);
    }

}

