import java.io.*;

public class WeekArray {
    public static void main(String[] args) {
        String[] week = {
            "“ú—j“ú",
            "Œ—j“ú",
            "‰Î—j“ú",
            "…—j“ú",
            "–Ø—j“ú",
            "‹à—j“ú",
            "“y—j“ú",
        };
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.println("0`6‚Ì”š‚ğ“ü—Í‚µ‚Ä‚­‚¾‚³‚¢B‘Î‰‚µ‚½—j“ú‚ğ•\¦‚µ‚Ü‚·B");
            String line = reader.readLine();
            int n = Integer.parseInt(line);
            if (0 <= n && n < 7) {
                System.out.println(week[n]);
            } else {
                System.out.println("0`6‚Ì”ÍˆÍ‚Å“ü—Í‚µ‚Ä‚­‚¾‚³‚¢");
            }
        } catch (IOException e) {
            System.out.println(e);
        } catch (NumberFormatException e) {
            System.out.println("”š‚ğ“ü—Í‚µ‚Ä‚­‚¾‚³‚¢");
        }
    }
}
