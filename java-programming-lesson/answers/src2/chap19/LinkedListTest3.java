import java.util.*;

public class LinkedListTest3 {
    public static void main(String[] args) {
        // QueueとしてLinkedListのインスタンスを確保する
        Queue<String> queue = new LinkedList<String>();

        // 要素の追加(offerメソッド)
        queue.offer("Alice");
        System.out.println("offer後のqueue = " + queue);
        queue.offer("Bob");
        System.out.println("offer後のqueue = " + queue);
        queue.offer("Chris");
        System.out.println("offer後のqueue = " + queue);
        queue.offer("Diana");
        System.out.println("offer後のqueue = " + queue);

        try {
            while (true) {
                // 先頭要素の参照(elementメソッド)
                String name = queue.element();
                System.out.println("elementの戻り値 = " + name);

                // 要素の抽出と削除(removeメソッド)
                name = queue.remove();
                System.out.println("removeの戻り値 = " + name);
                System.out.println("remove後のqueue = " + queue);
            }
        } catch (NoSuchElementException e) {
            e.printStackTrace();
        }
    }
}
