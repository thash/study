public class GcTest1 {
    public static void main(String[] args) {
        while (true) {
            String s = new String("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
            System.out.println("c‚èƒƒ‚ƒŠ = " + Runtime.getRuntime().freeMemory());
        }
    }
}
