import java.io.*;

public class RenameFile2 {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("使用法：java RenameFile2 現在のファイル名 新しいファイル名");
            System.out.println("例：java RenameFile2 old.txt new.txt");
            System.exit(0);
        }
        String oldfilename = args[0];
        String newfilename = args[1];
        File oldfile = new File(oldfilename);
        File newfile = new File(newfilename);
        if (!oldfile.exists()) {
            System.out.println(oldfilename + "が見つかりません。");
        } else if (newfile.exists()) {
            System.out.println(newfilename + "はすでに存在します。");
        } else if (oldfile.renameTo(newfile)) {
            System.out.println(oldfilename + "を" + newfilename + "に変更しました。");
        } else {
            System.out.println(oldfilename + "を" + newfilename + "に変更できませんでした。");
        }
    }
}
