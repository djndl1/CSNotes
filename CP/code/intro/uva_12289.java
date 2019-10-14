import java.util.Scanner;

class Main {
  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);

    int n = in.nextInt();
    String str;
    while (n-- > 0) {
      str = in.next();

      if (str.length() == 5)
        System.out.println(3);
      else if (str.charAt(0) == 'o') {
        if (str.charAt(1) == 'w' && str.charAt(2) == 'o')
          System.out.println(2);
        else
          System.out.println(1);
      } else if (str.charAt(0) == 't') {
        if (str.charAt(1) == 'n' && str.charAt(2) == 'e')
          System.out.println(1);
        else
          System.out.println(2);
      } else {
          if (str.charAt(1) == 'w')
              System.out.println(2);
          else 
          System.out.println(1);
      }

    }
  }
}
