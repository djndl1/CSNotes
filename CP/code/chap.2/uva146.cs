using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;

class UVA146
{
    private static string ReadCode(StreamReader fin)
    {
        var temp = fin.ReadLine().Trim();
        while (temp.Length == 0)
            temp = fin.ReadLine().Trim();

        return temp;
    }

    private static bool NextPermutation<T>(List<T> perm)
        where T : IComparable<T>
    {
        int k = perm.Count - 2;
        while (k >= 0)
        {
            if (perm[k].CompareTo(perm[k+1]) < 0)
                break;
            k--;
        }
        if (k < 0)
            return false;

        int l = perm.Count - 1;
        while (l > k)
        {
            if (perm[l].CompareTo(perm[k]) > 0)
                break;
            l--;
        }
        T temp = perm[l];
        perm[l] = perm[k];
        perm[k] = temp;

        perm.Reverse(k+1, perm.Count-k-1);

        return true;
    }



    public static void Main()
    {
        using (var cin = new StreamReader(Console.OpenStandardInput()))
            using (var cout = new StreamWriter(Console.OpenStandardOutput()))
            {
                while (true)
                {
                    string code = ReadCode(cin);
                    if (code == null || code == "#")
                        break;
                    var codeSeq = code.ToCharArray().ToList();
                    if (NextPermutation(codeSeq))
                    {
                        String successor = String.Join("", codeSeq);
                        cout.WriteLine(successor);
                    }
                    else
                    {
                        cout.WriteLine("No Successor");
                    }

                }

            }
    }
}
