public class Kamoku {
    String namae;   // 科目名
    int tensuu;     // 点数

    // コンストラクタ
    public Kamoku(String namae, int tensuu) {
        this.namae = namae;
        this.tensuu = tensuu;
    }

    // 科目の文字列表現
    public String toString() {
        return namae + "は" + tensuu + "点";
    }
}
