using System;
using System.Collections.Generic;
using System.Linq;

class UVA11222
{
    private static SortedSet<int> ReadProblems()
    {
        var lineText = Console.ReadLine();
        while (String.IsNullOrWhiteSpace(lineText))
            lineText = Console.ReadLine();

        var line = lineText.Split(new char[] {' '}, StringSplitOptions.RemoveEmptyEntries)
            .Select((s) => Convert.ToInt32(s))
            .ToList();
        line.RemoveAt(0);

        return new SortedSet<int>(line);
    }

    private static SortedSet<int> UniqueProblems(SortedSet<int> bloated, SortedSet<int> a, SortedSet<int> b)
    {
        var temp = bloated.Except(a);

        return new SortedSet<int>(temp.Except(b));
    }

    public static void Main()
    {
        int T = Convert.ToInt32(Console.ReadLine());

        for (int i = 0; i < T; i++)
        {
            var first = ReadProblems();
            var second = ReadProblems();
            var third = ReadProblems();

            var firstUnique = UniqueProblems(first, second, third);
            var secondUnique = UniqueProblems(second, first, third);
            var thirdUnique = UniqueProblems(third, first, second);

            int greatest = new int[] {firstUnique.Count, secondUnique.Count, thirdUnique.Count}.Max();

            Console.WriteLine($"Case #{i+1}:");
            var sets = new List<SortedSet<int>> {firstUnique, secondUnique, thirdUnique};
            for (int p = 0; p < 3; p++)
            {
                if (sets[p].Count == greatest)
                {
                    Console.Write($"{p+1} {greatest}");

                    foreach (var problemNum in sets[p])
                    {
                        Console.Write($" {problemNum}");
                    }
                    Console.WriteLine();
                }
            }

        }
    }
}
