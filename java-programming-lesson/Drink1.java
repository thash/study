import java.io.*;

public class Drink1 {
  public static void main(String[] args) {
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    try {
      System.out.println("Whick drink do you like ?");
      System.out.println("1 Orange Juice");
      System.out.println("2 Coffee");
      System.out.println("3 Milk.");
      System.out.println("choose 1, 2, or 3.");
      String line = reader.readLine();
      int n = Integer.parseInt(line);
      switch (n) {
        case 1:
          System.out.println("Orange Juice");
          break;
        case 2:
          System.out.println("Coffee");
          break;
        case 3:
          System.out.println("Milk");
          break;
        default:
          System.out.println("Neither.");
          break;
      }
    } catch (IOException e) {
      System.out.println(e);
    } catch (NumberFormatException e) {
      System.out.println("Wrong number.");
    }
  }
}

      
