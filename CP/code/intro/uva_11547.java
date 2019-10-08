import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        
        Scanner in = new Scanner(System.in);
        int t = in.nextInt();
        
        while (t-- > 0) {
            long n = in.nextInt();
        n = n * 567;
        n /= 9;
        n += 7492;
        n *= 235;
        n /= 47;
        n -= 498;

        if (n >= 10) {
            System.out.printf("%d\n", (n / 10) % 10);
        } else if (n <= -10)
            System.out.printf("%d\n", (-n / 10) % 10);
        else
            System.out.printf("0\n");
        }
    }
}
