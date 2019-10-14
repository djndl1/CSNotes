import java.util.Scanner;

class Main {
  public static void main(String[] args) {
        int i = 0;
        Scanner in = new Scanner(System.in);

        while (true) {
            int n = in.nextInt();

            if (n == 0)
                return;

            int reasons = 0;
            int treats = 0;
            while (n-- > 0) {
                int tmp = in.nextInt();
                
                if (tmp == 0)
                    treats++;
                else if (tmp > 0 && tmp < 100)
                    reasons++;
        }
        System.out.printf("Case %d: %d\n", ++i, reasons - treats);
            
        }
    }

}
