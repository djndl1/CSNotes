import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        int t, n;

        Scanner in = new Scanner(System.in);
        t = in.nextInt();
        while (t-- > 0) {
            n = in.nextInt();
            int max = 0, min = 100;
            int tmp;
            while (n-- > 0) {
                tmp = in.nextInt();
                if (tmp > max) max = tmp;
                if (tmp < min) min = tmp;
            }
            System.out.println(2 * (max - min));
        }
    }
    
}
