using System;
using System.Linq;
using System.Collections.Generic;

class UVA11942
{
    static void Main(string[] args)
    {
        string tempLine = Console.ReadLine();
        int N = int.Parse(tempLine);
        
        Console.WriteLine("Lumberjacks:");
        while (N-- > 0) 
        {
            bool ascend = true, descend = true;
            var lens = Console.ReadLine()
                             .Split(' ')
                             .Select(((s) => int.Parse(s)))
                             .ToArray();
            int prev = lens[0];
            foreach (int cur in lens)
            {
                if (cur < prev)
                    ascend = false;
                else if (cur > prev)
                    descend = false;
                prev = cur;
            }

            Console.WriteLine(ascend || descend ? "Ordered" : "Unordered");
        }
    }
}