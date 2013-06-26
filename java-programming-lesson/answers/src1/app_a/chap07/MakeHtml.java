import java.io.*;

public class MakeHtml {
    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            String line;
            // ルール1
            System.out.println("<html><head><title>My Page</title></head><body>");
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("■")) {
                    // ルール3
                    System.out.println("<h1>" + line.substring(1) + "</h1>");
                } else if (line.startsWith("●")) {
                    // ルール4
                    System.out.println("<h2>" + line.substring(1) + "</h2>");
                } else if (line.equals("----")) {
                    // ルール5
                    System.out.println("<hr>");
                } else if (line.equals("address")) {
                    // ルール6
                    System.out.println("<a href=\"mailto:hyuki@example.com\">hyuki@example.com</a>");
                } else {
                    // ルール7
                    System.out.println(line);
                }
            }
            // ルール2
            System.out.println("</body></html>");
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
