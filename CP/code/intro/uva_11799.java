import java.util.*;
import java.io.*;
import java.util.stream.Collectors;


class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader term = new BufferedReader(new InputStreamReader(System.in));
        String temp = term.readLine();
        int T = Integer.decode(temp);
        final int origT = T;
        while (T-- > 0) {
            temp = term.readLine();
            ArrayList<String> ints = new ArrayList<String>(Arrays.asList(temp.split(" ")));
            ints.remove(0);
            int speed = ints.stream()
                            .mapToInt(i -> Integer.decode(i))
                            .max()
                            .getAsInt();
            System.out.printf("Case %d: %d\n", origT - T, speed);
        }
    }
}