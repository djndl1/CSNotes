import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        int m, n, k;
        Scanner in = new Scanner(System.in);
        k = in.nextInt();

        while (k-- > 0) {
            m = in.nextInt();
            n = in.nextInt();

            System.out.println((n/3) * (m/3));
        }
    }
}
