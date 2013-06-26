public class Heikin5 {
    public static void main(String[] args) {
        int[] ten = { 63, 90, 75, 45, 81 };
        int sum = 0;
        for (int i = 0; i < ten.length; i++) {
            sum += ten[i];
        }
        double heikin = (double)sum / ten.length;
        System.out.println("国語は" + ten[0] + "点");
        System.out.println("数学は" + ten[1] + "点");
        System.out.println("英語は" + ten[2] + "点");
        System.out.println("理科は" + ten[3] + "点");
        System.out.println("社会は" + ten[4] + "点");
        System.out.println("平均点は" + heikin + "点");
    }
}
