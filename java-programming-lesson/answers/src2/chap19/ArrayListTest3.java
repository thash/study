import java.util.*;

public class ArrayListTest3 {
    public static void main(String[] args) {
        // ArrayListのインスタンスの確保
        ArrayList<String> list = new ArrayList<String>();

        // 要素の追加(addメソッド)
        list.add("Alice");
        list.add("Bob");
        list.add("Chris");
        list.add("Diana");
        list.add("Elmo");

        // イテレータを使ったforループ
        for (Iterator<String> it = list.iterator(); it.hasNext(); ) {
            String name = it.next();
            System.out.println(name);
        }
    }
}
