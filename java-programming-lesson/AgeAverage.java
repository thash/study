import java.io.*;

public class AgeAverage {
  public static void main(String[] args) {
    
    System.out.println("What's the first name ?");
    
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    try {
      String name = reader.readLine();
      System.out.println("How old ?");
      String line = reader.readLine();
      int age = Integer.parseInt(line);

      System.out.println("What's the second name ?");
      String name2 = reader.readLine();
      System.out.println("How old ?");
      String line2 = reader.readLine();
      int age2 = Integer.parseInt(line2);

      System.out.println("You are " + name + " and " + name2 + ". Hello !");
      System.out.println( name + " is " + age + ", and " + name2 + " is " + age2 + ", so your average age is " + ((age + age2)/2) + "years.");
    } catch (IOException e) {
      System.out.println(e);
    } catch (NumberFormatException e) {
      System.out.println("Wrong age.");
    }

  }
}
