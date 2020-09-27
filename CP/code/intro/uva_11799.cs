using System;
using System.Linq;
using System.Collections.Generic;


class UVA11799
{
    public static void Main()
    {
        string temp = Console.ReadLine();
        int T = int.Parse(temp);

        int origT = T;
        while (T-- > 0) 
        {
            temp = Console.ReadLine();
            var ints = temp.Split(new string[] {" "}, 
                                 StringSplitOptions.RemoveEmptyEntries)
                            .Select((si) => int.Parse(si)).ToList();
            ints.RemoveAt(0);
            int speed = ints.Max();
            Console.WriteLine($"Case {origT-T}: {speed}");
        }
    }
}