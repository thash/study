public class StudentTest {
    public static void main(String[] args) {
        Student[] data = {
            new Student("Œ‹é_", 65, 90, 100),
            new Student("ˆ¢•”˜a”n",  82,  73,  64),
            new Student("ˆÉ“¡Œõˆê",  74,  31,  42),
            new Student("²“¡‘¾˜Y", 100,  95,  99),
        };
        for (int i = 0; i < data.length; i++) {
            System.out.println("" + data[i] + "\t-> " + data[i].total());
        }
    }
}
