public class OneBankTest extends Thread {
    public void run() {
        while (true) {
            // 100‰~—a‚¯“ü‚ê
            OneBank.addMoney(100);
            // 100‰~ˆø‚«o‚µ
            OneBank.addMoney(-100);
        }
    }
    public static void main(String[] args) {
        new OneBankTest().start();
        new OneBankTest().start();
    }
}
