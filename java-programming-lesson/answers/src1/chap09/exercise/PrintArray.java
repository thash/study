public class PrintArray {
    public static void main(String[] args) {
        int[][] arr = {
            { 3, 1, 4, 1, },
            { 5, 9, 2, },
            { 6, 5, },
            { 3, },
        };
        printArray(arr);
    }
    public static void printArray( ??? ) {
        System.out.println("{");
        for (int i = 0; ???; i++) {
            System.out.print( ??? );
            for (int j = 0; ???; j++) {
                System.out.print( ??? );
            }
            System.out.println( ??? );
        }
        System.out.println("}");
    }
}
