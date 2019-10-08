import java.util.Scanner;

class Main {
  public static void main(String[] args) {
    int K;

    Scanner in = new Scanner(System.in);
    while (true) {
      K = in.nextInt();
      if (K == 0)
        return;

      int N = in.nextInt();
      int M = in.nextInt();
      while (K-- > 0) {
        int resX = in.nextInt();
        int resY = in.nextInt();

        if (resX > N && resY > M) {
          System.out.printf("NE\n");
        } else if (resX > N && resY < M) {
          System.out.printf("SE\n");
        } else if (resX < N && resY > M) {
          System.out.printf("NO\n");
        } else if (resX < N && resY < M) {
          System.out.printf("SO\n");
        } else
          System.out.printf("divisa\n");
      }
    }
  }

}
