import java.io.*;

public class ShowArgs {
public static void main(String[] args){
    System.out.println("value of args.length is " + args.length);
    for (int i=0; i < args.length; i++) {
        System.out.println("value of args[" + i + "] is " + args[i] );
    }
    }
}
