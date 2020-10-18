using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;

class LifeTime
{
    public uint birthYear
    {
        get; set;
    }
    public uint deathYear
    {
        get; set;
    }

    public LifeTime(uint by, uint dy)
    {
        birthYear = by;
        deathYear = dy;
    }
}


class CF1424G
{
    public static void Main()
    {
        using (var cin = new StreamReader(Console.OpenStandardInput()))
            using (var cout = new StreamWriter(Console.OpenStandardOutput()))
            {
                var line = cin.ReadLine().Trim();
                int n = Convert.ToInt32(line);

                var population = new List<LifeTime>(n);
                var checkPoints = new SortedSet<uint>();
                for (int p = 0; p < n; p++)
                {
                    line = cin.ReadLine().Trim();
                    var yrs = line.Split(' ');
                    var person = new LifeTime(UInt32.Parse(yrs[0]), UInt32.Parse(yrs[1]));
                    population.Add(person);

                    checkPoints.Add(person.birthYear);
                    checkPoints.Add(person.deathYear);
                }


                uint maxYear = 0;
                int maxPopulation = 0;
                foreach (uint curYear in checkPoints)
                {
                    int curPopulation = population.Where(l => l.birthYear <= curYear && curYear < l.deathYear)
                        .Count();
                    if (curPopulation > maxPopulation)
                    {
                        maxYear = curYear;
                        maxPopulation = curPopulation;
                    }
                }
                cout.WriteLine(maxYear + " " + maxPopulation);
            }
    }
}
