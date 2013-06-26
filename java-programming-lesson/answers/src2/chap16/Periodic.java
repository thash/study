public class Periodic {
    public static void main(String[] args) {
        for (int i = 0; i < 10; i++) {
            int tm = i * 1000;
            System.out.println("Start sleep:tm = " + tm);
            try {
                Thread.sleep(tm);
            } catch (InterruptedException e) {
            }
        }
    }
}
