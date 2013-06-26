public class GcArray1 {
    public static void main(String[] args) {
        while (true) {
            int[] a = new int[1000];    
            for (int i = 0; i < 1000; i++) {
                a[i] = i;
            }
            System.out.println("Žc‚èƒƒ‚ƒŠ = " + Runtime.getRuntime().freeMemory());
        }
    }
}
