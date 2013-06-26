interface DebugPrintable {
    int NO_ERROR = 0;
    int FILE_ERROR = 1;
    int MEMORY_ERROR = 2;
    String PREFIX = "ERROR:";
    void debugPrint();
}
