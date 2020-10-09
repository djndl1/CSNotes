// https://www.ics.uci.edu/~eppstein/161/960125.html
import java.util.*;
import java.io.*;

class Main {
    private static <T extends Comparable<? super T>> int partition(List<T> list) {
        T pivot = list.get(list.size()-1);
        int j = -1;
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).compareTo(pivot) > 0) {
                j++;
                T temp = list.get(i);
                list.set(i, list.get(j));
                list.set(j, temp);
            }
        }
        T temp = list.get(j+1);
        list.set(j+1, list.get(list.size()-1));
        list.set(list.size()-1, temp);

        return (j+1);
    }

    public static <T extends Comparable<? super T>> T quickSelect(List<T> list, int n) {
        int pivotIndex = partition(list);
        if (n <= pivotIndex)
            return quickSelect(list.subList(0, pivotIndex), n);
        else if (n > pivotIndex + 1)
            return quickSelect(list.subList(pivotIndex + 1, list.size()), pivotIndex);
        return list.get(pivotIndex);
    }

   public static void main(String[] args) throws IOException {
       try (BufferedReader cin = new BufferedReader(new InputStreamReader(System.in));
            PrintWriter cout = new PrintWriter(System.out)) {
          
       }
   }
}
