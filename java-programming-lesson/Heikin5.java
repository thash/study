import java.io.*;

public class Heikin5 {
public static void main(String[] args){
    int[] ten = {63, 90, 75, 45, 81};
    int sum = 0;
    for (int i = 0; i < ten.length; i++ ) {
        sum += ten[i];
    }
    double heikin = (double)sum / ten.length;
    System.out.println("Language: " + ten[0] );
    System.out.println("Math: " + ten[1] );
    System.out.println("English: " + ten[2] );
    System.out.println("Science: " + ten[3] );
    System.out.println("Society: " + ten[4] );
    System.out.println("Average: " + heikin );

    }
}
