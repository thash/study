public class GoodBankTest extends Thread {
    GoodBank bank;
    public GoodBankTest(GoodBank bank) {
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
        GoodBank bank = new GoodBank();   // GoodBank‚ğ¶¬
        new GoodBankTest(bank).start();
        new GoodBankTest(bank).start();
    }
}
