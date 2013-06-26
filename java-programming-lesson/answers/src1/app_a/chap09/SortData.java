public class SortData {
    public static void main(String[] args) {
        int[] data = { 31, 41, 59, 26, 53, 58, 97, 93, 23, 84 };
        System.out.println("•À‚×‘Ö‚¦‚é‘O");
        for (int i = 0; i < data.length; i++) {
            System.out.print(data[i] + " ");
        }
        System.out.println("");
        for (int i = 0; i < data.length - 1; i++) {
            for (int j = i + 1; j < data.length; j++) {
                if (data[i] > data[j]) {
                    // ŒðŠ·
                    int a = data[i];
                    data[i] = data[j];
                    data[j] = a;
                }
            }
        }
        System.out.println("•À‚×‘Ö‚¦‚½Œã");
        for (int i = 0; i < data.length; i++) {
            System.out.print(data[i] + " ");
        }
        System.out.println("");
    }
}
