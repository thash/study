class RunnableJob extends Job implements Runnable {
    public RunnableJob(int n) {
        super(n);
    }
    public void run() {
        while (true) {
            work();
        }
    }
}

public class MultiThreadProgram2 {
    public MultiThreadProgram2(int jobcount) {
        for (int i = 0; i < jobcount; i++) {
            new Thread(new RunnableJob(i)).start();
        }
    }
    public static void main(String[] args) {
        new MultiThreadProgram2(10);
    }
}
