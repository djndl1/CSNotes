import java.time.format.DateTimeFormatter;
import java.time.LocalDate;
import java.util.Scanner;

class GetWeekday {
    public static void main(String[] args) {
        Scanner sin = new Scanner(System.in);
        int y = sin.nextInt();
        int m = sin.nextInt();
        int d = sin.nextInt();
        var date = LocalDate.of(y, m, d);
        var Eformatter = DateTimeFormatter.ofPattern("E");
        String s = date.format(Eformatter);
        System.out.println(s);

    }
    
}
