using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

class UVA11988
{
    private static void ListPrepend<T>(LinkedList<T> orig, LinkedList<T> toPrepend)
    {
        var back = toPrepend.Last;
        while (back != null)
        {
            orig.AddFirst(back.Value);
            back = back.Previous;
        }
        toPrepend.Clear();
    }

    public static void Main()
    {
        string line;
        var rin = new StreamReader(Console.OpenStandardInput());
        var wout = new StreamWriter(Console.OpenStandardOutput());
        while ((line = rin.ReadLine()) != null)
        {
            var beiju = new LinkedList<char>();
            var temp = new LinkedList<char>();

            bool appending = true;
            foreach (char c in line)
            {
                if (c == '[' && appending)
                {
                    appending = false;
                    continue;
                }
                else if (c == '[' && !appending)
                {
                    ListPrepend(beiju, temp);
                    continue;
                }
                else if (c == ']' && !appending)
                {
                    appending = true;
                    ListPrepend(beiju, temp);
                    continue;
                }
                else if (c == ']' && appending)
                {
                    continue;
                }

                if (appending)
                {
                    beiju.AddLast(c);
                }
                else
                {
                    temp.AddLast(c);
                }
            }

            if (!appending)
                ListPrepend(beiju, temp);

            foreach (char c in beiju)
                wout.Write(c);
            wout.Write(Environment.NewLine);
        }
    }
}
