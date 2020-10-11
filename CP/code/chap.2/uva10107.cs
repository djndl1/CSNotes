using System;
using System.Linq;
using System.Collections.Generic;
using System.IO;

class uva10107
{
    public static void Main()
    {
        using (var cin = new StreamReader(Console.OpenStandardInput()))
            using (var cout = new StreamWriter(Console.OpenStandardOutput()))
            {
                var nums = new long[10001];
                int size = 0;
                while (true)
                {
                    string line = cin.ReadLine();
                    if (line == null)
                        break;
                    long cur = Convert.ToInt64(line.Trim());
                    nums[size] = cur;
                    size++;
                    cout.Write(Median(nums, size) + Environment.NewLine);
                }
            }
    }

    private static long Max(long[] list, int size)
    {
        long m = list[0];
        for (int i = 0; i < size; i++)
            if (list[i] > m)
                m = list[i];
        return m;
    }

    public static long Median(long[] list, int count)
    {
        if (list == null)
            throw new ArgumentNullException();
        if (count == 0)
            throw new InvalidOperationException("List contains no element");

        long first = QuickSelect(list, count / 2, count);
        if (count % 2 == 0)
        {
            long second = Max(list, count / 2);
            return (first + second) / 2;
        }
        else
        {
            return first;
        }
    }

    public static T QuickSelect<T>(T[] list, int n, int count)
        where T : IComparable<T>
    {
        if (list == null)
            throw new ArgumentNullException();
        if (count == 0)
            throw new InvalidOperationException("List contains no element");
        if (n < 0 || n > count)
            throw new IndexOutOfRangeException();

        return _QuickSelect(list, n, 0, count);
    }

    private static T _QuickSelect<T>(T[] list, int n, int low, int high)
        where T : IComparable<T>
    {
        int pivotIndex = Partition(list, low, high);

        if (n < pivotIndex)
            return _QuickSelect(list, n, low, pivotIndex);
        else if (n > pivotIndex)
            return _QuickSelect(list, n, pivotIndex+1, high);
        else
            return list[pivotIndex];
    }

    private static int Partition<T>(T[] list, int low, int high)
        where T : IComparable<T>
    {
        T pivot = list[high-1];
        int j = low - 1;

        for (int i = low; i < high; i++)
        {
            if (list[i].CompareTo(pivot) < 0)
            {
                j++;
                T temp = list[i];
                list[i] = list[j];
                list[j] = temp;
            }
        }
        T tmp = list[j+1];
        list[j+1] = list[high-1];
        list[high-1] = tmp;

        return (j+1);
    }
}
