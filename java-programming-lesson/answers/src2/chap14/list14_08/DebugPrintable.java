interface DebugPrintable {
    public static final int NO_ERROR = 0;
    public static final int FILE_ERROR = 1;
    public static final int MEMORY_ERROR = 2;
    public static final String PREFIX = "ERROR:";
    public abstract void debugPrint();
}
