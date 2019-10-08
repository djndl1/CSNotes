import java.util.Scanner;

class Main  {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int T = in.nextInt();

        int i = 0;
        while (T-- > 0) {
            int a = in.nextInt();
            int b = in.nextInt();
            int c = in.nextInt();
        

        int mid;
        if (a > b && a > c && b > c)
            mid = b;
        else if (a > b && a > c && c > b)
            mid = c;
        else if (b > a && b > c && a > c)
            mid = a;
        else if (b > a && b > c && c > a)
            mid = c;
        else if (c > a && c > b && a > b)
            mid = a;
        else
            mid = b;

        System.out.println("Case " + (++i) + ": " + mid);
        }

    }
    
}
