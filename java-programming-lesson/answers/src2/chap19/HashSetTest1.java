import java.util.*;

public class HashSetTest1 {
    public static void main(String[] args) {
        // SetとしてHashSetのインスタンスを確保する
        Set<String> set = new HashSet<String>();

        // 要素の追加(addメソッド)
        set.add("Alice");
        set.add("Bob");
        set.add("Chris");
        set.add("Diana");
        set.add("Elmo");

        // 拡張forループ
        for (String name : set) {
            System.out.println(name);
        }

        // Aliceは含まれているか？
        if (set.contains("Alice")) {
            System.out.println("setにAliceは含まれています。");
        } else {
            System.out.println("setにAliceは含まれていません。");
        }
    }
}
