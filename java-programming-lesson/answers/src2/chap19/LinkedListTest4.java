import java.util.*;

public class LinkedListTest4 {
    public static void main(String[] args) {
        // LinkedListのインスタンスを確保する
        LinkedList<String> stack = new LinkedList<String>();

        // 要素の追加(addFirstメソッド)
        stack.addFirst("Alice");
        System.out.println("addFirst後のstack = " + stack);
        stack.addFirst("Bob");
        System.out.println("addFirst後のstack = " + stack);
        stack.addFirst("Chris");
        System.out.println("addFirst後のstack = " + stack);
        stack.addFirst("Diana");
        System.out.println("addFirst後のstack = " + stack);
        stack.addFirst("Elmo");
        System.out.println("addFirst後のstack = " + stack);

        try {
            while (true) {
                // 先頭要素の参照(getFirstメソッド)
                String name = stack.getFirst();
                System.out.println("getFirstの戻り値 = " + name);

                // 要素の抽出と削除(removeFirstメソッド)
                name = stack.removeFirst();
                System.out.println("removeFirstの戻り値 = " + name);
                System.out.println("removeFirst後のstack = " + stack);
            }
        } catch (NoSuchElementException e) {
            e.printStackTrace();
        }
    }
}
