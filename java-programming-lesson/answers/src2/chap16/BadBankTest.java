public class BadBankTest extends Thread {
    BadBank bank;
    public BadBankTest(BadBank bank) {
        this.bank = bank;
    }
    public void run() {
        while (true) {
            // 100‰~—a‚¯“ü‚ê
            bank.addMoney(100);
            // 100‰~ˆø‚«o‚µ
            bank.addMoney(-100);
        }
    }
    public static void main(String[] args) {
        BadBank bank = new BadBank();   // BadBank‚ğ¶¬
        new BadBankTest(bank).start();
        new BadBankTest(bank).start();
    }
}
