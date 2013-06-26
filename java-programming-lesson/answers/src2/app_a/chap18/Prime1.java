import java.io.*;

public class Prime1 {
    static final int MAX_PRIME = 1000;
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("使用法：java Prime1 作成ファイル");
            System.out.println("例：java Prime1 prime.txt");
            System.exit(0);
        }
        String filename = args[0];
        try {
            PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter(filename)));
            writePrime(writer);
            writer.close();
        } catch (FileNotFoundException e) {
            System.out.println(filename + "が見つかりません。");
        } catch (IOException e) {
            System.out.println(e);
        }
    }
    public static void writePrime(PrintWriter writer) {
        boolean[] prime = new boolean[MAX_PRIME];
        for (int n = 0; n < MAX_PRIME; n++) {
            prime[n] = true;
        }
        prime[0] = false;
        prime[1] = false;
        for (int n = 0; n < MAX_PRIME; n++) {
            if (prime[n]) {
                writer.println(n);
                for (int i = 2; i * n < MAX_PRIME; i++) {
                    prime[i * n] = false;
                }
            }
        }
    }
}
