class MyojiNamae {
    String myoji;
    String namae;
}

public class MethodTest4 {
    public static MyojiNamae getYourName() {
        MyojiNamae obj = new MyojiNamae();
        obj.myoji = "åãèÈ";
        obj.namae = "ç_";
        return obj;
    }
    public static void main(String[] args) {
        MyojiNamae mn = getYourName();
        System.out.println("ñºéöÇÕ" + mn.myoji);
        System.out.println("ñºëOÇÕ" + mn.namae);
    }
}
