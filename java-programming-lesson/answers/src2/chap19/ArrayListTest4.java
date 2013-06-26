import java.util.*;

public class ArrayListTest4 {
    public static void main(String[] args) {
        // ArrayListのインスタンスの確保
        ArrayList<String> list = new ArrayList<String>();

        // 要素の追加(addメソッド)
        list.add("Alice");
        list.add("Bob");
        list.add("Chris");
        list.add("Diana");
        list.add("Elmo");

        // 拡張forループ
        for (String name : list) {
            System.out.println(name);
        }
    }
}
