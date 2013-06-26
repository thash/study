public class ShowMaxData {
    public static void main(String[] args) {
        int[] data = { 31, 41, 59, 26, 53, 58, 97, 93, 23, 84 };
        int max_data = data[0];
        for (int i = 0; i < data.length; i++) {
            if (max_data < data[i]) {
                max_data = data[i];
            }
        }
        System.out.println("Å‘å’l‚Í" + max_data + "‚Å‚·B");
    }
}
