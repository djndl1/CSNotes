import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int i = 0;
        while(in.hasNext()) {
            String tmp = in.next();
            if (tmp.equals("#"))
                return;
            i++;

            if (tmp.equals("HELLO"))
                System.out.println("Case " + i + ": ENGLISH");
            else if (tmp.equals("HOLA"))
                System.out.println("Case " + i + ": SPANISH");
            else if (tmp.equals("HALLO"))
                System.out.println("Case " + i + ": GERMAN");
            else if (tmp.equals("BONJOUR"))
                System.out.println("Case " + i + ": FRENCH");
            else if (tmp.equals("CIAO"))
                System.out.println("Case " + i + ": ITALIAN");
            else if (tmp.equals("ZDRAVSTVUJTE"))
                System.out.println("Case " + i + ": RUSSIAN");
            else
                System.out.println("Case " + i + ": UNKNOWN");
        }
    }
}
