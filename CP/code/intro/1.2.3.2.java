import java.util.Scanner;
/**
 * 1.2.3.2
 */
class PrintPi {
    public static void main(String[] args) {
        Scanner sin = new Scanner(System.in);
            while (sin.hasNextInt()) {
                int precision = sin.nextInt();
                String formatString = "%." + precision + "f\n";
                System.out.printf(formatString, Math.PI);
            }
    }
}