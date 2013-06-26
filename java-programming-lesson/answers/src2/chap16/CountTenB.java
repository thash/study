public class CountTenB implements Runnable {
    public static void main(String[] args) {
        CountTenB ct = new CountTenB();
        Thread th = new Thread(ct);
        th.start();
        for (int i = 0; i < 10; i++) {
            System.out.println("main:i = " + i);
        }
    }
    public void run() {
        for (int i = 0; i < 10; i++) {
            System.out.println("run:i = " + i);
        }
    }
}
