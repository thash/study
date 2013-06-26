import java.io.*;

public class DrinkIf {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.println("飲み物は何が好きですか？");
            System.out.println("あ＝オレンジジュース");
            System.out.println("い＝コーヒー");
            System.out.println("う＝どちらでもない");
            System.out.println("あ、い、う、のどれかを選んでください。");
            String line = reader.readLine();
            if (line.equals("あ")) {
                System.out.println("オレンジジュースです。");
            } else if (line.equals("い")) {
                System.out.println("コーヒーです。");
            } else {
                System.out.println("どちらでもありません。");
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
