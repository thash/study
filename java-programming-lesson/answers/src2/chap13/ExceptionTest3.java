public class ExceptionTest3 {
    public static void main(String[] args) {
        int[] myarray = new int[3]; 
        try {
            System.out.println("‘ã“ü‚µ‚Ü‚·");
            myAssign(myarray, 100, 0);
            System.out.println("‘ã“ü‚µ‚Ü‚µ‚½");
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("‘ã“ü‚Å‚«‚Ü‚¹‚ñ‚Å‚µ‚½");
            System.out.println("—áŠO‚Í" + e + "‚Å‚·");
        }
        System.out.println("I—¹‚µ‚Ü‚·");
    }
    static void myAssign(int[] arr, int index, int value) {
        System.out.println("myAssign‚É—ˆ‚Ü‚µ‚½");
        arr[index] = value;
        System.out.println("myAssign‚©‚ç‹A‚è‚Ü‚·");
    }
}
