public class JoinTest extends Thread {
    public static void main(String[] args) {
        JoinTest th = new JoinTest();
        System.out.println("main:はじめ");
        th.start();
        System.out.println("main:終了待ちに入る");
        try {
            th.join();
        } catch (InterruptedException e) {
            System.out.println(e);
        }
        System.out.println("main:おわり");
    }
    public void run() {
        System.out.println("run:スレッド実行開始");
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            System.out.println(e);
        }
        System.out.println("run:スレッド実行終了");
    }
}
