import java.io.*;

public class Heikin6 {
    public static void main(String[] args){
        int[][] tens = {
            {63, 90, 75, 45, 81},
            {85, 100, 95, 80, 90},
            {100,100,100,100,100},
        };
        for (int i = 0; i < tens.length; i++) {
            int sum = 0;
            for (int k = 0; k < tens[i].length; k++ ) {
                System.out.print("\t" + tens[i][k]);
                sum += tens[i][k];
            }
           System.out.println("\t|" + (double)sum/tens[i].length);

        }
    }
}
