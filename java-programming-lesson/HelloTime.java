import java.io.* ;

public class HelloTime {
  public static void main(String[] args) {
    System.out.println("What time is it now ?");
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

    try {
      String line = reader.readLine();
      System.out.println("now " + line + ",so...");
      int n = Integer.parseInt(line);

      if (n >= 0 && n <= 11 ) {
        System.out.println("AM. Good Morning ! ");
      } else if (n == 12 ) {
        System.out.println("NOON !");
      } else if (n >= 13 && n <= 18 ) {
        System.out.println("PM. Good Afternoon ! ");
      } else if (n >= 19 && n <= 23 ) {
        System.out.println("Night. Good Night ! ");
      } else {
        System.out.println("Wrong Time. 0 - 23 ");
      }
    } catch (IOException e) {
      System.out.println(e);
    } catch (NumberFormatException e) {
      System.out.println("Wrong number.");
    }
  }
}
