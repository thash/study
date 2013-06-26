import java.io.*;

public class Heikin2 {
public static void main(String[] args){
    int[] ten;
    double heikin;

    ten = new int[3];

    ten[0] = 63;
    ten[1] = 90;
    ten[2] = 75;
    heikin = (ten[0] + ten[1] + ten[2]) / 3.0;

    System.out.println("Language: " + ten[0] );
    System.out.println("Math: " + ten[1] );
    System.out.println("English: " + ten[2] );
    System.out.println("Average: " + heikin );



    }
}
