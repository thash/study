public class SelectGreeting {
    public static void main(String[] args) {
        String[] message;

        message = new String[3];
        message[0] = "おはよう！";
        message[1] = "こんにちは！";
        message[2] = "こんばんは！";
        if (args.length != 1) {
            System.out.println("使い方：java SelectGreeting 番号");
            System.exit(0);
        }
        int num = Integer.parseInt(args[0]);
        if (0 <= num && num < message.length) {
            System.out.println(message[num]);
        } else {
            System.out.println("番号は0～" + 2 + "の範囲で指定してください。");
        }
    }
}
