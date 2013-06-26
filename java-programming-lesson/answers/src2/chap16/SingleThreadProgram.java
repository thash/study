public class SingleThreadProgram {
    Job[] jobs; 
    public SingleThreadProgram(int jobcount) {
        jobs = new Job[jobcount];
        for (int i = 0; i < jobcount; i++) {
            jobs[i] = new Job(i);
        }
    }
    public void workAllJobs() {
        for (int i = 0; i < jobs.length; i++) {
            jobs[i].work();
        }
    }
    public static void main(String[] args) {
        SingleThreadProgram self = new SingleThreadProgram(10);
        while (true) {
            self.workAllJobs();
        }
    }
}
