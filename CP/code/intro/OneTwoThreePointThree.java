import java.time.LocalDate;
import java.util.Scanner;

class  OneTwoThreePointThree {
    public static void main(String[] args) {
        var in = new Scanner(System.in);
        int y = in.nextInt();
        int m = in.nextInt();
        int d = in.nextInt();

        var day = LocalDate.of(y, m, d);
        System.out.println(day.getDayOfWeek());
    }
}
