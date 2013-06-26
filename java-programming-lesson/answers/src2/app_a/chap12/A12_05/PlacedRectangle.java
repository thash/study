class PlacedRectangle {
    Rectangle r;
    int x;
    int y;
    PlacedRectangle() {
        r = new Rectangle();
        setLocation(0, 0);
    }
    PlacedRectangle(int x, int y) {
        r = new Rectangle();
        setLocation(x, y);
    }
    PlacedRectangle(int x, int y, int width, int height) {
        r = new Rectangle(width, height);
        setLocation(x, y);
    }
    void setLocation(int x, int y) {
        this.x = x;
        this.y = y;
    }
    public String toString() {
        return "[ (" + x + ", " + y + ") " + r + "]";
    }
    public static void main(String[] args) {
        PlacedRectangle a = new PlacedRectangle();
        PlacedRectangle b = new PlacedRectangle(12, 34);
        PlacedRectangle c = new PlacedRectangle(31, 41, 59, 26);
        PlacedRectangle d = new PlacedRectangle(100, 200, 3, 4);
        d.setLocation(1, 2);
        System.out.println("a = " + a);
        System.out.println("b = " + b);
        System.out.println("c = " + c);
        System.out.println("d = " + d);
    }
}
