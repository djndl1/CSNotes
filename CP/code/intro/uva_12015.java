import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

class Main {
   public static void main(String[] args) {
       try (Scanner sin = new Scanner(System.in)) {
           int T = sin.nextInt();

           for (int t = 1; t <= T; t++) {
               LinkedHashMap<String, Integer> urlRelev = new LinkedHashMap<String, Integer>();
               int highest = Integer.MIN_VALUE;
               for (int i = 0; i < 10; i++) {
                   String curUrl = sin.next();
                   int curRelev = sin.nextInt();
                   urlRelev.put(curUrl, curRelev);

                   highest = highest < curRelev ? curRelev : highest;
               }
               System.out.printf("Case #%d:\n", t);
               for (Map.Entry<String, Integer> entry : urlRelev.entrySet()) {
                   if (entry.getValue().equals(highest))
                       System.out.println(entry.getKey());
               }
           }

       }
   }
}
