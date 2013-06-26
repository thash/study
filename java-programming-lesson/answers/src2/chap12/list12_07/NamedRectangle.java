class NamedRectangle extends Rectangle {
    String name;
    NamedRectangle() {
        this("NO NAME");    // 自分のクラスのコンストラクタの呼び出し
    }
    NamedRectangle(String name) {
        super(200, 32);
        this.name = name;
    }
}
