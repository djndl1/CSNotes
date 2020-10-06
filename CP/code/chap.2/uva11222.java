import java.io.*;
import java.util.*;
import java.util.stream.*;

class Main {
    private static TreeSet<Integer> readProblemsForOne(BufferedReader in) throws IOException {
        String lineText = in.readLine();
        while (lineText.isEmpty())
            lineText = in.readLine();

        String[] line = lineText.split(" ");

        TreeSet<Integer> problems = new TreeSet<Integer>();
        for (int i = 1; i < line.length; i++) {
                problems.add(Integer.decode(line[i]));
        }
        return problems;
    }

    private static TreeSet<Integer> getUniqueProblems(TreeSet<Integer> bloated, TreeSet<Integer>... others) {
        TreeSet<Integer> temp = new TreeSet<Integer>(bloated);
        for (TreeSet<Integer> set : others) {
            temp.removeAll(set);
        }

        return temp;
    }


   public static void main(String[] args) throws IOException {
       try (BufferedReader bin = new BufferedReader(new InputStreamReader(System.in));
            PrintWriter wout = new PrintWriter(System.out)) {
           int T = Integer.decode(bin.readLine());

           for (int t = 0; t < T; t++) {
               ArrayList<TreeSet<Integer>> problem_sets = new ArrayList<>();
               problem_sets.add(readProblemsForOne(bin));
               problem_sets.add(readProblemsForOne(bin));
               problem_sets.add(readProblemsForOne(bin));

               ArrayList<TreeSet<Integer>> unique_sets = new ArrayList<>();
               unique_sets.add(getUniqueProblems(problem_sets.get(0), problem_sets.get(1), problem_sets.get(2)));
               unique_sets.add(getUniqueProblems(problem_sets.get(1), problem_sets.get(2), problem_sets.get(0)));
               unique_sets.add(getUniqueProblems(problem_sets.get(2), problem_sets.get(1), problem_sets.get(0)));

               int greatest = unique_sets.stream().mapToInt(set -> set.size()).max().getAsInt();
               wout.print("Case #" + (t+1) +  ":" + System.lineSeparator());
               for (int i = 0; i < 3; i++) {
                   if (unique_sets.get(i).size() == greatest) {
                       wout.print((i+1) + " " + greatest);
                       for (Integer probID : unique_sets.get(i))
                           wout.print(" " + probID);
                       wout.print(System.lineSeparator());
                   }
               }
           }
       }
   }
}
