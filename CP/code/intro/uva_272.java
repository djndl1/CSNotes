import java.util.Scanner;

class Main {
    public static void main(String[] args) {
        String tmp;
        boolean quote = false;

        Scanner in = new Scanner(System.in);

        while (in.hasNext()) {
            tmp = in.nextLine();
            for (char c : tmp.toCharArray()) {
                if (c == '"') {
                    System.out.printf("%s", quote ? "''" : "``");
                    quote = !quote;
                } else
                    System.out.print(c);
            }
                System.out.print('\n');
        }
    }            
}
