public class Periodic2 {
    public static void main(String[] args) {
        for (int i = 0; i < 10; i++) {
            int tm = i * 1000;
            System.out.println("Start sleep:tm = " + tm);
            Thread.sleep(tm);
        }
    }
}
