public class CountTenA extends Thread {
    public static void main(String[] args) {
        CountTenA ct = new CountTenA();
        ct.start();
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
