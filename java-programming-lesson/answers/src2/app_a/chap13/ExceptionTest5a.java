public class ExceptionTest5a {
    public static void main(String[] args) {
        try {
            method1(0);
            method2(0);
            method3(0);
        } catch (Exception e) {
            System.out.println("—áŠO:" + e);
            e.printStackTrace();
        }
    }
    static void method1(int x) throws Exception {
        if (x > 0) {
            throw new Exception();
        }
    }
    static void method2(int x) throws Exception {
        if (x == 0) {
            throw new Exception();
        }
    }
    static void method3(int x) throws Exception {
        if (x > 0) {
            throw new Exception();
        }
    }
}
