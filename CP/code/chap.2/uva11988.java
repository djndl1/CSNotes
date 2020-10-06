import java.util.*;
import java.io.*;

class Main {
   public static void main(String[] args) throws IOException {
       try (BufferedReader bin = new BufferedReader(new InputStreamReader(System.in));
            PrintWriter pout = new PrintWriter(System.out)) {
           String line;
           while ((line = bin.readLine()) != null) {
               LinkedList<Character> beiju = new LinkedList<>();
               LinkedList<Character> temp = new LinkedList<>();
               Boolean appending = true;
               for (int cind = 0; cind < line.length(); cind++) {
                   char c = line.charAt(cind);
                   if (c == '[' && appending) {
                       appending = false;
                       continue;
                   } else if (c == '[' && !appending) {
                       beiju.addAll(0, temp);
                       temp.clear();
                       continue;
                   } else if (c == ']' && !appending) {
                       appending = true;
                       beiju.addAll(0, temp);
                       temp.clear();
                       continue;
                   } else if (c == ']' && appending) {
                       continue;
                   }

                   if (appending) {
                       beiju.addLast(c);
                   } else {
                       temp.addLast(c);
                   }
               }

               if (!appending) {
                       beiju.addAll(0, temp);
               }

               for (Character c : beiju) {
                   pout.print(c);
               }
               pout.print(System.lineSeparator());
           }
       }
   }
}
