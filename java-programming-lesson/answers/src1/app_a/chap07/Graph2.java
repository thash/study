public class Graph2 {
    public static void main(String[] args) {
        int i = 0;
        while (i < 10) {
            int j = 0;
            while (j < i * i) {
                System.out.print("*");
                j++;
            }
            i++;
	        System.out.println("");
        }
    }
}
