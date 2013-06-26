import java.util.*;

public class PrintfTest {
    public static void main(String[] args) {
        int x = 12345;
        int y = 67;
        int z = -890;
        String s = "Hello";

        // 2005-09-07 08:30:59
        Calendar cal = Calendar.getInstance();
        cal.set(2005, 9 - 1, 7, 8, 30, 59);

        System.out.printf("x = %d, y = %d, z = %d（10進数表示）%n", x, y, z);
        System.out.printf("x = %#x, y = %#x, z = %#x（16進数表示）%n", x, y, z);

        System.out.printf("%-10d円（10桁で左寄せ）%n", x);
        System.out.printf("%+-10d円（10桁で左寄せで符号付き）%n", x);
        System.out.printf("%10d円（10桁で右寄せ）%n", x);
        System.out.printf("%+10d円（10桁で右寄せで符号付き）%n", x);
        System.out.printf("%,10d円（10桁で右寄せで区切り付き）%n", x);

        System.out.printf("%tF（日付）%n", cal);
        System.out.printf("%tT（時刻）%n", cal);
        System.out.printf("%ta（曜日）%n", cal);
        System.out.printf("%tF (%<ta) %<tT （まとめた日付）%n", cal);
    }
}
