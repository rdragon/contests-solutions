// 2018-04-07
// go gopher, code jam 2018, qualification round

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeJam2018
{
    class GoGopher
    {
        static void Main() => Run();

        static void Run()
        {
            var t = GetInts()[0];
            for (int i = 0; i < t; i++)
                Solve();
        }

        static void Solve()
        {
            var a = GetInts()[0];
            var i = 1;
            while (true)
            {
                var bools = new bool[9];
                while (bools.Any(z => !z))
                {
                    Console.WriteLine($"{i + 1} 2");
                    Console.Out.Flush();
                    var xs = GetInts();
                    if (xs[0] == 0 && xs[1] == 0)
                        return;
                    if (xs[0] == -1 && xs[1] == -1)
                        return;
                    var row = xs[0] - i;
                    var column = xs[1] - 1;
                    bools[row * 3 + column] = true;
                }
                i += 3;
            }
        }

        static List<int> GetInts()
        {
            return Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToList();
        }
    }
}
