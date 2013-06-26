import java.util.*;

public class ArrayListTest5 {
    public static void main(String[] args) {
        // ArrayListのインスタンスの確保（型パラメータがない）
        ArrayList list = new ArrayList();

        // 要素の追加(addメソッド)
        list.add("Alice");
        list.add("Bob");
        list.add("Chris");
        list.add("Diana");
        list.add("Elmo");

        // イテレータを使ったループ（型パラメータがない）
        for (Iterator it = list.iterator(); it.hasNext(); ) {
            String name = (String)it.next();    // キャストが必要
            System.out.println(name);
        }
    }
}
