import java.io.*;

public class PrintArray {
    public static void main(String[] args){
        int[][] arr = {
            {3,1,4,1,},
            {5,9,2,},
            {6,5,},
            {3,},
        };
        printArray(arr);
    }

    public static void printArray(int[][] arr) {
        System.out.println("{");
        for (int i = 0; i < arr.length; i++) {
            System.out.print("  { ");
            for (int k = 0; k < arr[i].length; k++) {
                System.out.print(arr[i][k] + ", ");
            }
            System.out.println("},");
        }
        System.out.println("}");

    }
}
