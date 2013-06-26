public class Heikin7 {
    public static void main(String[] args) {
        Kamoku[] kamoku = {
            new Kamoku("Japanese", 63),
            new Kamoku("Math", 90),
            new Kamoku("English", 75),
            new Kamoku("Science", 45),
            new Kamoku("Society", 81), 
        };
        int sum = 0;
        for (int i = 0; i < kamoku.length; i++) {
            System.out.println(kamoku[i]);
            sum += kamoku[i].tensuu;
        }
        double heikin = (double)sum / kamoku.length;
        System.out.println("The average is " + heikin + "“_");
    }
}

