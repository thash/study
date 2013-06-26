import java.io.*;

public class MakeHtml {
    public static void main(String[] args){
        System.out.println("<html><head><title>My Page</title></head></body>");
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        try {
            String line;
            int linenum = 1;
            while ((line = reader.readLine()) != null ) {
      //          int n = line.indexOf(findstring);
                if (line.equals("----")) {
                    System.out.println("<hr>");
                    continue;
                } else if (line.startsWith("*")) {
                    System.out.println("<h1>" + line.substring(1) + "</h1>");
                    continue;
                } else if (line.startsWith("+")) {
                    System.out.println("<h2>" + line.substring(1) + "</h2>");
                    continue;
                } else if (line.equals("address")) {
                    System.out.println("<a href=\"mailto:hyuki@example.com\">hyuki@example.com</a>");
                    continue;
                }

                    System.out.println(line);
            }
            System.out.println("</body></html>");

        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
