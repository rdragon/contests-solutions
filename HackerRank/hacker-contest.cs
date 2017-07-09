// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HackerContest
{
    class Program
    {
        static int N;
        static long D;
        static List<long> Problems;
        static long[,] SolveTimes;
        static void Main(string[] args)
        {
            var t = int.Parse(Console.ReadLine());
            for (int i = 0; i < t; i++)
            {
                SolveTimes = new long[3000 + 1, 3000 + 1];
                var pair = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
                N = pair[0];
                D = pair[1];
                Problems = Console.ReadLine().Split(' ').Select(z => long.Parse(z)).ToList();
                Problems.Sort();
                SolveTimes[1, 1] = Problems[0];
                for (int problems = 2; problems <= N; problems++)
                {
                    var hardestProblem = Problems[problems - 1];
                    SolveTimes[problems, problems] = hardestProblem;
                    for (int knowers = problems - 1; knowers > Math.Max(problems / 2, 1); knowers--)
                    {
                        SolveTimes[knowers, problems] = Math.Max(hardestProblem, SolveTimes[knowers - 1, problems - 1]);
                    }
                    for (int knowers = problems / 2; knowers >= 2; knowers--)
                    {
                        SolveTimes[knowers, problems] = Math.Min(Math.Max(hardestProblem, SolveTimes[knowers - 1, problems - 1]),
                            D + SolveTimes[knowers * 2, problems]);
                    }
                    SolveTimes[1, problems] = D + SolveTimes[2, problems];
                }
                Console.WriteLine(SolveTimes[1, N]);
            }
        }
    }
}
