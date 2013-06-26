import java.io.*;

public class ListDir1 {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("使用法：java ListDir1 ディレクトリ名");
            System.out.println("例：java ListDir1 doc");
            System.exit(0);
        }
        String dirname = args[0];
        File dir = new File(args[0]);
        String[] dirlist = dir.list(); 
        for (int i = 0; i < dirlist.length; i++) {
            System.out.println(dirlist[i]);
        }
    }
}
