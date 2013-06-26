import java.io.*;

public class SortData {
    public static void main(String[] args){
        int[] data = {31, 41, 59, 26, 53, 58, 97, 93, 23, 84};
        System.out.println("Before");
        for (int i = 0; i < data.length; i++) {
            System.out.print(data[i] + " ");
        }
        System.out.println("");
        int x;
        for (int i = 0; i < data.length; i++) {
            for (int k = i + 1; k < data.length; k++) {
                if (data[i] > data[k]) {
                    x = data[i];
                    data[i] = data[k];
                    data[k] = x;
                }
            }
        }
        System.out.println("After");
        for ( int i = 0; i < data.length; i++) {
            System.out.print(data[i] + " ");
        }
        System.out.println("");

    }
}
