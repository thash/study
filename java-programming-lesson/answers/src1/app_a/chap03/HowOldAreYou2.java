import java.io.*;

public class HowOldAreYou2 {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            String line;
            // 1人目
            System.out.println("1人目の名前を入力してください。");
            String name1 = reader.readLine();
            System.out.println(name1 + "さんの年齢を入力してください。");
            line = reader.readLine();
            int age1 = Integer.parseInt(line);
            // 2人目
            System.out.println("2人目の名前を入力してください。");
            String name2 = reader.readLine();
            System.out.println(name2 + "さんの年齢を入力してください。");
            line = reader.readLine();
            int age2 = Integer.parseInt(line);
            // 平均
            double average = (age1 + age2) / 2.0;
            System.out.println(name1 + "さんと" + name2 + "さんの年齢の平均は" + average + "です。");
        } catch (IOException e) {
            System.out.println(e);
        } catch (NumberFormatException e) {
            System.out.println("年齢が正しくありません。");
        }
    }
}
