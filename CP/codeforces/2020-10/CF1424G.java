import java.util.*;
import java.io.*;

public class CF1424G {
    public static void main(String[] args) throws IOException{
        try (var cin = new BufferedReader(new InputStreamReader(System.in));
             var cout = new PrintWriter(System.out)) {
            var line = cin.readLine().trim();
            int n = Integer.parseInt(line);

            var incrementOfYear = new TreeMap<Integer, Integer>();
            for (int p = 0; p < n; p++) {
                line = cin.readLine().trim();
                var yrs = line.split(" ");
                Integer by = Integer.parseInt(yrs[0]);
                Integer dy = Integer.parseInt(yrs[1]);

                if (incrementOfYear.containsKey(by)) {
                    incrementOfYear.put(by, incrementOfYear.get(by) + 1);
                } else {
                    incrementOfYear.put(by, 1);
                }

                if (incrementOfYear.containsKey(dy)) {
                    incrementOfYear.put(dy, incrementOfYear.get(dy) - 1);
                } else {
                    incrementOfYear.put(dy, -1);
                }
            }

            Integer runningSum = 0;
            long maxYear = 0;
            long maxPopulation = 0;
            for (var pair : incrementOfYear.entrySet()) {
                runningSum += pair.getValue();
                if (runningSum > maxPopulation) {
                    maxPopulation = runningSum;
                    maxYear = pair.getKey();
                }
            }

            cout.write(maxYear + " " + maxPopulation + System.lineSeparator());
        }
    }
}
