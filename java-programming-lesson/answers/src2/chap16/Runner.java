class Runner extends Thread {
    private boolean running = true;
    public void stopRunning() {
        running = false;
    }
    public void run() {
        while (running) {
            doCommand();
        }
    }
}
