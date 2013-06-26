public class NoGoodBank {
    // 預金残高
    private int value = 0;
    // 実行中チェック？
    private boolean busy = false;
    // 預け入れ・引き出し
    public void addMoney(int money) {
        while (busy) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
            }
        }
        // 実行中
        busy = true;
        // 現在残高を保存
        int currentValue = value;
        // 状況を表示
        System.out.println(Thread.currentThread() + "が addMoney に入りました。");
        // 現在残高を変更
        value += money;
        // 矛盾がないかどうかチェック
        if (currentValue + money != value) {
            System.out.println(Thread.currentThread() + "で矛盾が発生しました！");
            System.exit(-1);
        }
        // 状況を表示
        System.out.println(Thread.currentThread() + "が addMoney から出ました。");
        // 実行中ではない
        busy = false;
    }
}
