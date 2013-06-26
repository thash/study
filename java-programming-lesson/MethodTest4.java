class MyojiNamae {
    String myoji;
    String namae;
}

public class MethodTest4 {
    public static MyojiNamae getYourName() {
        MyojiNamae obj = new MyojiNamae();
        obj.myoji = "yuuki";
        obj.namae = "hiroshi";
        return obj;
    }
    public static void main(String[] args) {
        MyojiNamae mn = getYourName();
        System.out.println("myoji is " + mn.myoji);
        System.out.println("namae is " + mn.namae);
    }

}

