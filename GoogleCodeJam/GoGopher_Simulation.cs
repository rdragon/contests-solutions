// 2018-04-08
// go gopher, code jam 2018, qualification round

// this is not the solution, but to check our probability computation
// the calculation gives as probability of success of one testcase p = 0.476710473504817 if we can only dig 580 times
// this program gives: simulations = 50007702, p = 0.476722485668308

using System;
using System.Linq;

namespace CodeJam2018
{
    class GoGopher_Simulation
    {
        static void Main() => Run();

        static void Run()
        {
            var r = new Random();
            var printInterval = TimeSpan.FromSeconds(10);
            var total = 0;
            var solved = 0;
            var nextPrint = DateTimeOffset.Now.Add(printInterval);
            while (true)
            {
                total++;
                if (Solve(r))
                    solved++;
                if (nextPrint < DateTimeOffset.Now)
                {
                    Console.WriteLine($"simulations = {total}, p = {(solved + 0.0) / total}");
                    nextPrint = DateTimeOffset.Now.Add(printInterval);
                }
            }
        }

        static bool Solve(Random r)
        {
            var n = 580;
            var requiredBlockCount = 23;
            var blocks = 0;
            var bools = new bool[9];
            for (int i = 0; i < n; i++)
            {
                bools[r.Next(9)] = true;
                if (bools.All(z => z))
                {
                    blocks++;
                    if (blocks == requiredBlockCount)
                        return true;
                    bools = new bool[9];
                }
            }
            return false;
        }
    }
}
