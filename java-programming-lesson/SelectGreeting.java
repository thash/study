import java.io.*;

public class SelectGreeting {
    public static void main(String[] args){
        String[] message = {"Good Morning !", "Good AfterNoon !", "Good Evening !" };


        if (args.length != 1) {
            System.out.println("How to use: java SelectGreeting No");
            System.exit(0);
        } 
        int num = Integer.parseInt(args[0]);
        if (0 <= num && num < message.length ) {
            System.out.println(message[num]);
        } else {
            System.out.println("Choose No 0 to 2.");
        };
    }
}
