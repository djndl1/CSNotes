import java.util.Scanner;

class OneTwoThreeTen {
    public static void main(String[] args) {
        var in = new Scanner(System.in);
        String tmp = in.nextLine();
        tmp = tmp.replaceAll("\\b[a-z][0-9][0-9]\\b", "***");
        System.out.println(tmp);
    }
}
