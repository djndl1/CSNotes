using System;
using System.Runtime.CompilerServices;

class ReadDouble 
{
    public static void Main()
    {
        string textLine = null;
        while ((textLine = Console.ReadLine()) != null)
        {
            var items = textLine.Split(new string[] {",", " ", "\t"}, StringSplitOptions.RemoveEmptyEntries);
            foreach (var item in items)
            {
                if (Double.TryParse(item, out double d))
                {
                    Console.WriteLine("{0,7:F3}", d);
                }
            }
        }
    }
}
