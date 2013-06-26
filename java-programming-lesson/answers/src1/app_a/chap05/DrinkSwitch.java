import java.io.*;

public class DrinkSwitch {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.println("飲み物は何が好きですか？");
            System.out.println("あ＝オレンジジュース");
            System.out.println("い＝コーヒー");
            System.out.println("う＝どちらでもない");
            System.out.println("あ、い、う、のどれかを選んでください。");
            String line = reader.readLine();
            char c = line.charAt(0);
            switch (c) {
            case 'あ':
                System.out.println("オレンジジュースです。");
                break;

            case 'い':
                System.out.println("コーヒーです。");
                break;

            default:
                System.out.println("どちらでもありません。");
                break;
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
