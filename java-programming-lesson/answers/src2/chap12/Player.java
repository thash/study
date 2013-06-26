abstract class Player {
    public abstract void play();
    public void loop(int n) {
        for (int i = 0; i < n; i++) {
            play();
        }
    }
}
