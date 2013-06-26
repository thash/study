import java.io.*;

public class WeekSwitch {
  public static void main(String[] args) {
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    try {
      System.out.println("enter 0 to 6");

      String line = reader.readLine();
      int n = Integer.parseInt(line);
      switch (n) {
        case 0 :
          System.out.println("Sunday");
          break;
        case 1 :
          System.out.println("Monday");
          break;
        case 2 :
          System.out.println("Tuesday");
          break;
        case 3 :
          System.out.println("Wednesday");
          break;
        case 4 :
          System.out.println("Thursday");
          break;
        case 5 :
          System.out.println("Friday");
          break;
        case 6 :
          System.out.println("Saturday");
          break;
        default:
          System.out.println("Neither");
          break;
      }
    } catch (IOException e) {
      System.out.println(e);
    } catch (NumberFormatException e) {
      System.out.println("Wrong number.");
    }
  }
}
