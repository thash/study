import java.io.*;

public class WeekSwitch {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.println("0`6‚Ì”š‚ğ“ü—Í‚µ‚Ä‚­‚¾‚³‚¢B‘Î‰‚µ‚½—j“ú‚ğ•\¦‚µ‚Ü‚·B");
            String line = reader.readLine();
            int n = Integer.parseInt(line);
            switch (n) {
            case 0:
                System.out.println("“ú—j“ú");
                break;
            case 1:
                System.out.println("Œ—j“ú");
                break;
            case 2:
                System.out.println("‰Î—j“ú");
                break;
            case 3:
                System.out.println("…—j“ú");
                break;
            case 4:
                System.out.println("–Ø—j“ú");
                break;
            case 5:
                System.out.println("‹à—j“ú");
                break;
            case 6:
                System.out.println("“y—j“ú");
                break;
            default:
                System.out.println("0`6‚Ì”ÍˆÍ‚Å“ü—Í‚µ‚Ä‚­‚¾‚³‚¢");
                break;
            }
        } catch (IOException e) {
            System.out.println(e);
        } catch (NumberFormatException e) {
            System.out.println("”š‚ğ“ü—Í‚µ‚Ä‚­‚¾‚³‚¢");
        }
    }
}
