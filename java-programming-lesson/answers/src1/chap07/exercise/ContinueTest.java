import java.io.*;

public class ContinueTest {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.endsWith("{") && !line.endsWith("}")) {
                    continue;
                }
                System.out.println(line);
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
