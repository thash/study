import java.io.*;

public class Greeting {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.println("現在の時刻を入力してください（「時」のみでけっこうです）。");
            String line = reader.readLine();
            int n = Integer.parseInt(line);
            if (0 <= n && n <= 11) {
                System.out.println("おはようございます");
            } else if (n == 12) {
                System.out.println("お昼です");
            } else if (13 <= n && n <= 18) {
                System.out.println("こんにちは");
            } else if (19 <= n && n <= 23) {
                System.out.println("こんばんは");
            } else {
                System.out.println("時刻の範囲を越えています");
            }
        } catch (IOException e) {
            System.out.println(e);
        } catch (NumberFormatException e) {
            System.out.println("数字の形式が正しくありません。");
        }
    }
}
