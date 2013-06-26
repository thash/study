import java.io.*;

public class DrinkIF2 {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
          System.out.println("Whick drink do you like ?");
      System.out.println("A Orange Juice");
      System.out.println("B Coffee");
            System.out.println("C Neither.");
      System.out.println("choose A, B, or C.");
            String line = reader.readLine();
            char c = line.charAt(0);
            switch (c) {
              case 'A':
                System.out.println("Orange Juice");
              break;
              case 'B':
                System.out.println("Coffee");
              break;
              default:
                System.out.println("Neither");
                break;
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
