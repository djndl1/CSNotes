import java.util.*;

class Main {
   public static void main(String[] args) {
       var a = new LinkedList<Character>(Arrays.asList('a', 'b', 'c', 'd'));
       var b = new LinkedList<Character>(Arrays.asList('1', '2', '3', '4'));

       a.addAll(0, b);
       for (Character c : a)
           System.out.print(c);
       System.out.println();
   }
}
