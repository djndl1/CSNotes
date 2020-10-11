// https://www.ics.uci.edu/~eppstein/161/960125.html
import java.util.*;
import java.io.*;

class uva10107 {
    private static <T extends Comparable<? super T>> int partition(List<T> list) {
        T pivot = list.get(list.size()-1);
        int j = -1;
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).compareTo(pivot) < 0) {
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
        if (list.size() == 0)
            throw new NoSuchElementException();
        if (n < 0 || n >= list.size())
            throw new IndexOutOfBoundsException();

        int pivotIndex = partition(list);
        if (n < pivotIndex)
            return quickSelect(list.subList(0, pivotIndex), n);
        else if (n > pivotIndex)
            return quickSelect(list.subList(pivotIndex + 1, list.size()), n - pivotIndex - 1);
        else
            return list.get(pivotIndex);
    }

    public static Long median(List<Long> list) {
        Long first = quickSelect(list, list.size()/2);
        if (list.size() % 2 == 0) {
            Long second = Collections.max(list.subList(0, list.size()/2));
            return (second + first) / 2;
        } else {
            return first;
        }
    }

    private static int partition(long[] array, int low, int high) {
        long pivot = array[high-1];
        int j = low - 1;
        for (int i = low; i < high; i++) {
            if (array[i] < pivot) {
                j++;
                long temp = array[i];
                array[i] = array[j];
                array[j] = temp;
            }
        }
        long temp = array[j+1];
        array[j+1] = array[high-1];
        array[high-1] = temp;

        return (j+1);
    }

    private static long _quickSelect(long[] array, int n, int low, int high) {
        int pivotIndex = partition(array, low, high);

        if (n < pivotIndex)
            return _quickSelect(array, n, low, pivotIndex);
        else if (n > pivotIndex)
            return _quickSelect(array, n, pivotIndex + 1, high);
        else
            return array[pivotIndex];
    }

    public static long quickSelect(long[] array, int n, int length) {
        if (array.length == 0)
            throw new NoSuchElementException();
        if (n < 0 || n >= array.length)
            throw new IndexOutOfBoundsException();

        return _quickSelect(array, n, 0, length);
    }

    private static long max(long[] array, int low, int high) {
        long m = array[0];

        for (int i = low; i < high; i++) {
            if (m < array[i])
                m = array[i];
        }

        return m;
    }


    public static long median(long[] array, int length) {
        if (length == 0)
            throw new NoSuchElementException();
        long first = quickSelect(array, length / 2, length);
        if (length % 2 == 0) {
            long second = max(array, 0, length / 2);
            return (second + first) / 2;
        } else {
            return first;
        }
    }

   public static void main(String[] args) throws IOException {
       try (BufferedReader cin = new BufferedReader(new InputStreamReader(System.in));
            PrintWriter cout = new PrintWriter(System.out)) {
           long[] nums = new long[10001];
           int size = 0;
           while (true) {
               String line = cin.readLine();
               if (line == null)
                   break;
               long cur = Long.parseLong(line);
               nums[size] = cur;
               size++;

               cout.write(median(nums, size) + System.lineSeparator());
           }
       }
   }
}
