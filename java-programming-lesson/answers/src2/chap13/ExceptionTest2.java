 public class ExceptionTest2 {
     public static void main(String[] args) {
         int[] myarray = new int[3]; 
         try {
             System.out.println("‘ã“ü‚µ‚Ü‚·");
             myarray[100] = 0;
             System.out.println("‘ã“ü‚µ‚Ü‚µ‚½");
         } catch (ArrayIndexOutOfBoundsException e) {
             System.out.println("‘ã“ü‚Å‚«‚Ü‚¹‚ñ‚Å‚µ‚½");
             System.out.println("—áŠO‚Í" + e + "‚Å‚·");
         }
         System.out.println("I—¹‚µ‚Ü‚·");
     }
 }
