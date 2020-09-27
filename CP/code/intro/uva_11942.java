import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        try (Scanner sin = new Scanner(System.in)) {
            int N = sin.nextInt();

            System.out.println("Lumberjacks:");
            while (N-- > 0) {
                boolean ascend = true, descend = true;

                int prev = sin.nextInt();
                for (int i = 0; i < 9; i++) {
                    int cur = sin.nextInt();

                    if (cur > prev)
                        descend = false;
                    else if (cur < prev)
                        ascend = false;

                    prev = cur;
                }

                System.out.println(ascend || descend ? "Ordered" : "Unordered");
            }
        }
        
    }    
}
