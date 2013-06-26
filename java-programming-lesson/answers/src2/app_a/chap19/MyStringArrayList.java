public class MyStringArrayList {
    private static final int INITIAL_CAPACITY = 4;
    private String[] ar;
    private int sz;

    public MyStringArrayList() {
        ar = new String[INITIAL_CAPACITY];
        sz = 0;
    }

    public void add(String s) {
        if (ar.length == sz) {
            String[] newAr = new String[ar.length * 2];
            System.arraycopy(ar, 0, newAr, 0, ar.length);
            ar = newAr;
        }
        ar[sz] = s;
        sz++;
    }

    public String get(int n) {
        if (0 <= n && n < sz) {
            return ar[n];
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    public int size() {
        return sz;
    }
}
