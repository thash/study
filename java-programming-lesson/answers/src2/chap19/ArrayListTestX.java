import java.util.*;

public class ArrayListTestX {
    public static void main(String[] args) {
        // ListとしてArrayListのインスタンスを確保する
        List<String> list = new ArrayList<String>();

        // 要素の追加(addメソッド)
        list.add("Alice");
        list.add("Bob");
        list.add("Chris");
        list.add("Bob");
        list.add("Elmo");

        // 最初と最後の要素の参照(getメソッドとsizeメソッド)
        System.out.println("最初の要素 = " + list.get(0));
        System.out.println("最後の要素 = " + list.get(list.size() - 1));

        // すべての要素の参照
        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i));
        }

        // Bobの添字は？
        System.out.println("最初に出てくるBobの添字 = " + list.indexOf("Bob"));
        System.out.println("最後に出てくるBobの添字 = " + list.lastIndexOf("Bob"));
    }
}
