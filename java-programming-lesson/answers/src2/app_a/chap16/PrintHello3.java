class LabelPrinter extends Thread {
    String label = "no label";
    LabelPrinter(String label) {
        this.label = label;
    }
    public void run() {
        while (true) {
            System.out.println(label);
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
            }
        }
    }
}

public class PrintHello3 {
    public static void main(String[] args) {
        new LabelPrinter("Ç®ÇÕÇÊÇ§ÅI").start();
        new LabelPrinter("Ç±ÇÒÇ…ÇøÇÕÅI").start();
        new LabelPrinter("Ç±ÇÒÇŒÇÒÇÕÅI").start();
    }
}
