import java.util.Scanner;

class ReadDouble {
    public static void main(String[] args) {
        Scanner sin = new Scanner(System.in);
        double d;
        while (sin.hasNextDouble()) {
            d = sin.nextDouble();
            System.out.printf("%7.3f\n", d);
        }
        sin.close();
    }
}
