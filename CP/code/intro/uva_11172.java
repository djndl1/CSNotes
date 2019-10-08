import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        int n;
        long a, b;
        Scanner in = new Scanner(System.in);

        n = in.nextInt();
        while (n-- > 0) {
            a = in.nextInt();
            b = in.nextInt();
            System.out.println((a == b) ? '=' : ((a > b) ? '>' : '<'));
        }
        in.close();
    }
}
