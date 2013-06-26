import java.util.*;

public class GcArray2 {
    static ArrayList<int[]> list = new ArrayList<int[]>();
    public static void main(String[] args) {
        while (true) {
            int[] a = new int[1000];
            for (int i = 0; i < 1000; i++) {
                a[i] = i;
            }
            list.add(a);
            System.out.println("Žc‚èƒƒ‚ƒŠ = " + Runtime.getRuntime().freeMemory());
        }
    }
}
