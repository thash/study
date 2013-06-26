public class Heikin3 {
    public static void main(String[] args) {
        int[] ten;
        int sum;
        double heikin;

        ten = new int[3];
        ten[0] = 63;
        ten[1] = 90;
        ten[2] = 75;
        sum = 0;
        for (int i = 0; i < 3; i++) {
            sum = sum + ten[i];
        }
        heikin = sum / 3.0;

        System.out.println("国語は" + ten[0] + "点");
        System.out.println("数学は" + ten[1] + "点");
        System.out.println("英語は" + ten[2] + "点");
        System.out.println("平均点は" + heikin + "点");
    }
}
