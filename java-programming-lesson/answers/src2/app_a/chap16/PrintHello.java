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

public class PrintHello {
    public static void main(String[] args) {
        LabelPrinter th = new LabelPrinter("Ç±ÇÒÇ…ÇøÇÕÅI");
        th.start();
    }
}
