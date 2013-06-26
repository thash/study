import java.io.*;

class MyFileReader extends FileReader implements DebugPrintable { 
    String filename = null;
    public MyFileReader(String filename) throws FileNotFoundException {
        super(filename);
        this.filename = filename;
    }
    public void debugPrint() {
        System.out.println("MyFileReaderのインスタンス：ファイル名は" + filename + "です。");
    }
}
