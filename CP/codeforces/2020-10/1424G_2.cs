using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;

class CF1424G
{
    public static void Main()
    {
        using (var cin = new StreamReader(Console.OpenStandardInput()))
            using (var cout = new StreamWriter(Console.OpenStandardOutput()))
            {
                var line = cin.ReadLine().Trim();
                int n = Convert.ToInt32(line);

                var incrementOfYear = new SortedDictionary<uint, long>();
                for (int p = 0; p < n; p++)
                {
                    line = cin.ReadLine().Trim();
                    var yrs = line.Split(' ');
                    uint by = UInt32.Parse(yrs[0]);
                    uint dy = UInt32.Parse(yrs[1]);
                    if (incrementOfYear.ContainsKey(by))
                    {
                        incrementOfYear[by] += 1;
                    }
                    else
                    {
                        incrementOfYear.Add(by, 1);
                    }
                    if (incrementOfYear.ContainsKey(dy))
                    {
                        incrementOfYear[dy] -= 1;
                    }
                    else
                    {
                        incrementOfYear.Add(dy, -1);
                    }
                }
                long runningSum = 0;
                long maxPopulation = 0;
                long maxYear = 0;
                foreach (var pair in incrementOfYear)
                {
                    runningSum += pair.Value;
                    if (runningSum > maxPopulation) {
                        maxYear = pair.Key;
                        maxPopulation = runningSum;
                    }
                }

                cout.WriteLine(maxYear + " " + maxPopulation);
            }
    }
}
