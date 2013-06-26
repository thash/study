public class KamokuHeikin {
    public static void main(String[] args) {
        Kamoku[] kamoku = {
            new Kamoku("‘Œê", 63),
            new Kamoku("”Šw", 90),
            new Kamoku("‰pŒê", 75),
            new Kamoku("—‰È", 45),
            new Kamoku("Ğ‰ï", 81),
        };
        int sum = 0;
        for (int i = 0; i < kamoku.length; i++) {
            System.out.println(kamoku[i]);
            sum += kamoku[i].tensuu;
        }
        double heikin = (double)sum / kamoku.length;
        System.out.println("•½‹Ï“_‚Í" + heikin + "“_");
    }
}
