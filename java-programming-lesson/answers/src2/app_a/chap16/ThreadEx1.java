class ThreadAsterisk1 extends Thread {
    public void run() {
        for (int i = 0; i < 10; i++) {
            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
            }
            System.out.println("***");
        }
    }
}
class ThreadEqual1 extends Thread {
    public void run() {
        for (int i = 0; i < 10; i++) {
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
            }
            System.out.println("=====");
        }
    }
}
public class ThreadEx1 {
    public static void main(String[] args) {
        new ThreadAsterisk1().start();
        new ThreadEqual1().start();
    }
}
