public class ArrayTest {
    public static void main(String[] args) {
        // 配列の確保
        String[] array = new String[3];

        // 要素の代入
        array[0] = "Alice";
        array[1] = "Bob";
        array[2] = "Chris";

        // 要素の参照
        for (int i = 0; i < array.length; i++) {
            System.out.println(array[i]);
        }
    }
}
