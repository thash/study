public class GamePlayer {
    public static String playername;
    public GamePlayer(String name) {
        playername = name;
    }
    public String toString() {
        return "[player:" + playername + "]";
    }
    public static void main(String[] args) {
        GamePlayer[] player = new GamePlayer[3];
        player[0] = new GamePlayer("Mad Hatter");
        player[1] = new GamePlayer("March Hare");
        player[2] = new GamePlayer("Alice");
        for (int i = 0; i < player.length; i++) {
            System.out.println(player[i]);
        }
    }
}
