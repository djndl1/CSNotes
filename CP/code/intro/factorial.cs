using System;
using System.Numerics;

class Factorial 
{
    public static void Main(string[] args)
    {
        var fac = BigInteger.One;
        for (int i = 2; i <= 25; i++) 
        {
            fac = fac * i;
        }
        Console.WriteLine(fac);
    }
}