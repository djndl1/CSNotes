import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        int cur, first, second, third;
        
        Scanner in = new Scanner(System.in);
        cur = in.nextInt();
        first = in.nextInt();
        second = in.nextInt();
        third = in.nextInt();
        while ((cur != 0 || first != 0|| second != 0 || third != 0)) {
            System.out .println(1080 + ((cur + 40 - first) % 40 + (second + 40 - first) % 40 + (second + 40 - third) % 40) * 9);
            cur = in.nextInt();
            first = in.nextInt();
            second = in.nextInt();
            third = in.nextInt();

            
        }
    }
}
