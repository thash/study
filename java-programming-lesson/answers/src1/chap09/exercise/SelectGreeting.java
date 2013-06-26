public class SelectGreeting {
    public static void main(String[] args) {
        String[] message ???

        ???

        if (args.length != 1) {
            System.out.println("使い方：java SelectGreeting 番号");
            System.exit(0);
        }
        int num = Integer.parseInt( ??? );
        if (0 <= num && num < message.length) {
            System.out.println(message[num]);
        } else {
            System.out.println("番号は0～" + ??? + "の範囲で指定してください。");
        }
    }
}
