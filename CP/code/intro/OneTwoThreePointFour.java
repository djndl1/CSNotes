
import java.util.Arrays;
import java.util.HashSet;
import java.util.Scanner;

class OneTwoThreePointFour {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        var ints = new HashSet<>();
        while (in.hasNextInt()) {
            ints.add(in.nextInt());
        }
        var sorted = ints.toArray();
        Arrays.sort(sorted);
        for (var i : sorted)
            System.out.print(i + " ");
        System.out.print('\n');
    }
    
}
