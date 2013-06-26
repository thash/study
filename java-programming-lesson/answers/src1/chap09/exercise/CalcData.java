public class CalcData {
    public static void main(String[] args) {
        double[] data;
        data = new double[5];
        data[0] = 3.1;
        data[1] = 4.1;
        data[2] = 5.9;
        data[3] = 2.6;
        data[4] = 5.3;
        data[5] = 9.7;
        double sum = 0.0;
        for (int i = 0; i <= data.length; i++) {
            sum += data[i];
        }
        System.out.println("sum = " + sum);
    }
}
