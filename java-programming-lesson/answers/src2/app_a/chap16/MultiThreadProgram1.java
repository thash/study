class JobThread extends Thread {
    Job job;
    public JobThread(int n) {
        job = new Job(n);
    }
    public void run() {
        while (true) {
            job.work();
        }
    }
}

public class MultiThreadProgram1 {
    public MultiThreadProgram1(int jobcount) {
        for (int i = 0; i < jobcount; i++) {
            new JobThread(i).start();
        }
    }
    public static void main(String[] args) {
        new MultiThreadProgram1(10);
    }
}
