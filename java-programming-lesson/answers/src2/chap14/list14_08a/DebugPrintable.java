interface DebugPrintable {
    enum Type {
        NO_ERROR, FILE_ERROR, MEMORY_ERROR,
    };
    String PREFIX = "ERROR:";
    void debugPrint();
}
