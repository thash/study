import java.io.*;

public class StringCode {
  public static void main(String[] args) {
    
    System.out.println("Enter String.");
    
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    try {
      String line = reader.readLine();
      for (int i = 0; i < line.length(); i++) {
        char c = line.charAt(i);
        System.out.println("the character code of '" + c + "' is  " + (int)c + ".");
      }

    } catch (IOException e) {
      System.out.println(e);
    }
  }
}
