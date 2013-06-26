public class Heikin1 {
    public static void main(String[] args) {
        int kokugo, suugaku, eigo;
        double heikin;

        kokugo = 63;
        suugaku = 90;
        eigo = 75;
        heikin = (kokugo + suugaku + eigo) / 3.0;

        System.out.println("国語は" + kokugo + "点");
        System.out.println("数学は" + suugaku + "点");
        System.out.println("英語は" + eigo + "点");
        System.out.println("平均点は" + heikin + "点");
    }
}
