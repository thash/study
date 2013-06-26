class Rectangle {
    int width;
    int height;
    Rectangle() {
        setSize(10, 20);
    }
    Rectangle(int w, int h) {
        setSize(w, h);
    }
    void setSize(int w, int h) {
        width = w;
        height = h;
    }
    int getArea() {
        return width * height;
    }
    public static void main(String[] args) {
        Rectangle r1 = new Rectangle();
        System.out.println("r1.width = " + r1.width);
        System.out.println("r1.height = " + r1.height);
        System.out.println("r1.getArea() = " + r1.getArea());
        Rectangle r2 = new Rectangle(123, 45);
        System.out.println("r2.width = " + r2.width);
        System.out.println("r2.height = " + r2.height);
        System.out.println("r2.getArea() = " + r2.getArea());
    }
}
