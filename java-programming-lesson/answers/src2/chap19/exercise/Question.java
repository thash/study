import java.util.*;

public class Question {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<String>();

        list.add("Alice");
        list.add("Bob");
        list.add("Chris");
        // Å¶1

        System.out.println(list.get(2)); // Å¶2

        System.out.println(list.size()); // Å¶3

        for (Iterator<String> it = list.iterator(); it.hasNext(); ) {
            String name = it.next(); // Å¶4
            System.out.println(name);
        }

    }
}
