import java.io.*;

public class ShowUnicode {
    public static void main(String[] args) {
        System.out.println("文字列を入力してください。");
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            String line = reader.readLine();
            for (int i = 0; i < line.length(); i++) {
                char c = line.charAt(i);
                System.out.println("'" + c + "' の文字コードは " + (int)c + "です。");
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
