import java.util.*;

public class LinkedListTest2 {
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

        // 先頭要素の参照(peekメソッド)
        while (queue.peek() != null) {
            // 要素の抽出と削除(pollメソッド)
            String name = queue.poll();
            System.out.println("pollの戻り値   = " + name);
            System.out.println("poll後のqueue  = " + queue);
        }
    }
}
