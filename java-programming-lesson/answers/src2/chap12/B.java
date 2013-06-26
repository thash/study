class A {
    A() {
        System.out.println("1: A()");
    }
    A(int x) {
        System.out.println("2: A(int x)");
    }
}
class B extends A {
    B() {
        System.out.println("3: B()");
    }
    B(int x) {
        System.out.println("4: B(int x)");
    }
    B(String s) {
        super(789);
        System.out.println("5: B(String s)");
    }
    public static void main(String[] args) {
        System.out.println("-----");
        new A();
        System.out.println("-----");
        new B();
        System.out.println("-----");
        new A(123);
        System.out.println("-----");
        new B(456);
        System.out.println("-----");
        new B("test");
        System.out.println("-----");
    }
}
