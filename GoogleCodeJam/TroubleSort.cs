// 2018-04-07
// trouble sort, code jam 2018, qualification round

using System;
using System.Collections.Generic;
using System.Linq;

namespace CodeJam2018
{
    class TroubleSort
    {
        static void Main() => Run();

        static void Run()
        {
            var t = GetInts()[0];
            for (int i = 1; i <= t; i++)
            {
                Console.ReadLine();
                Console.WriteLine($"Case #{i}: {Solve(GetInts())}");
            }
        }

        static List<int> GetInts()
        {
            return Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToList();
        }

        static string Solve(List<int> xs)
        {
            var n = xs.Count;
            var ys = new List<int>();
            var zs = new List<int>();
            for (int i = 0; i < n; i += 2)
            {
                ys.Add(xs[i]);
                if (i + 1 < n)
                    zs.Add(xs[i + 1]);
            }
            ys.Sort();
            zs.Sort();
            xs.Clear();
            for (int i = 0; i < ys.Count; i++)
            {
                xs.Add(ys[i]);
                if (i < zs.Count)
                    xs.Add(zs[i]);
            }
            for (int i = 0; i < n - 1; i++)
                if (xs[i + 1] < xs[i])
                    return $"{i}";
            return "OK";
        }
    }
}
