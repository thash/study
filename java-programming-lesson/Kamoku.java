public class Kamoku {
    String namae;
    int tensuu;

    public Kamoku(String namae, int tensuu) {
        this.namae = namae;
        this.tensuu = tensuu;
    }

    public String toString() {
        return namae + "は" + tensuu + "点";
    }
}
