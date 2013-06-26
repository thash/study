import java.util.*;

public class HashMapTest1 {
    public static void main(String[] args) {
        // MapとしてHashMapのインスタンスを確保する
        Map<String,Integer> map = new HashMap<String,Integer>();

        // キーと値のペアを追加(putメソッド)
        map.put("Alice", 100);
        map.put("Bob", 57);
        map.put("Chris", 85);
        map.put("Diana", 85);
        map.put("Elmo", 92);

        // エントリに関する拡張forループ
        for (Map.Entry<String,Integer> entry : map.entrySet()) {
            System.out.println(entry.getKey() + " => " + entry.getValue());
        }
        System.out.println();

        // キーに関する拡張forループ
        for (String name : map.keySet()) {
            System.out.println(name);
        }
        System.out.println();

        // 値に関する拡張forループ
        for (int value : map.values()) {
            System.out.println(value);
        }
        System.out.println();

        // キーに関する拡張forループで値も得る
        for (String name : map.keySet()) {
            System.out.println(name + " => " + map.get(name));
        }
        System.out.println();

        // キーBobの値を得る
        System.out.println("Bobの値 = " + map.get("Bob"));

        // キーFredの値を得る
        System.out.println("Fredの値 = " + map.get("Fred"));
    }
}
