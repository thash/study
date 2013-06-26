public class MethodTest {
    /* "結城" と "浩" の2つの文字列を返すメソッド（？） */
    public static void getYourName(String myoji, String namae) {
        myoji = "結城";
        namae = "浩";
    }
    public static void main(String[] args) {
        String x;
        String y;
        getYourName(x, y);
        System.out.println("名字は" + x);
        System.out.println("名前は" + y);
    }
}
