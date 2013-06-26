import java.io.* ;

public class HowOldAreYou {
  public static void main(String[] args) {
    System.out.println("What's your name ?");
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    try {
      String line = reader.readLine();
      System.out.println("You are " + line + ". Hello !");
      System.out.println("How old are you ?");
      line = reader.readLine();
      int age = Integer.parseInt(line);
      System.out.println("Now you are " + age + ", so you will be " + (age + 10) + " ten years later.");
    } catch (IOException e) {
      System.out.println(e);
    } catch (NumberFormatException e) {
      System.out.println("Wrong age.");
    }
  }
}
