class NamedRectangle extends Rectangle {
    String name;
    NamedRectangle() {
        super();         // スーパークラスの引数なしコンストラクタの呼び出し
                         //（省略しても同じ）
        name = "NO NAME";
    }
    NamedRectangle(String name) {
        super(200, 32);  // スーパークラスの引数付きコンストラクタの呼び出し
                         //（省略するとsuper();になる）
        this.name = name;
    }
}
