import java.util.*;
import java.io.*;

class Main {
    private static String readCode(BufferedReader sin) throws IOException {
        String temp = sin.readLine().trim();
        while (temp.isEmpty())
            temp = sin.readLine().trim();

        return temp;
   }

   private static <E extends Comparable<? super E>> boolean nextPermutation(List<E> perm) {
       int k = perm.size() - 2;
       while (k >= 0) {
           if (perm.get(k).compareTo(perm.get(k+1)) < 0)
               break;
           k--;
       }
       if (k < 0)
           return false;

       int l = perm.size() - 1;
       while (l > k) {
           if (perm.get(l).compareTo(perm.get(k)) > 0)
               break;
           l--;
       }

       E temp = perm.get(l);
       perm.set(l, perm.get(k));
       perm.set(k, temp);

       Collections.reverse(perm.subList(k+1, perm.size()));

       return true;
    }

   public static void main(String[] args) throws IOException {
       try (BufferedReader sin = new BufferedReader(new InputStreamReader(System.in));
            PrintWriter sout = new PrintWriter(System.out)) {
           while (true) {
               String code = readCode(sin);
               if (code == null || code.equals("#"))
                   break;

               ArrayList<String> codeList = new ArrayList<String>(Arrays.asList(code.split("")));
               if (nextPermutation(codeList)) {
                   String successor = String.join("", codeList);
                   sout.print(successor + System.lineSeparator());
               } else
                       sout.print("No Successor" + System.lineSeparator());
           }
       }
   } 
}
