public class Student {
    String name;    // –¼
    int[] tens;     // Œ±‚Ì“_”
    public Student(String name, int[] tens) {
        this.name = name;
        this.tens = tens;
    }
    public Student(String name, int x, int y, int z) {
        ???
    }
    public String toString() {
        String s = "[" + name;
        for (int i = 0; ???; i++) {
            s += ???;
        }
        s += "]";
        return s;
    }
    public int total() {
        int sum = 0;
        for (int i = 0; ???; i++) {
            ???
        }
        return sum;
    }
}
