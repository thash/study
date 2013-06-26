import java.io.*;

public class CopyLower {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            String line;
            while ((line = reader.readLine()) != null) {
                String s = line.toLowerCase();
                System.out.println(s);
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
