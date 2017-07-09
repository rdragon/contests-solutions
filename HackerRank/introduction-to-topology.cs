// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IntroductionToTopology
{
    class Solver
    {
        int N, M;
        List<short>[] Ls;
        short[,] X;
        int[] Mem;
        const int Modulo = 1000000007;

        internal void Start()
        {
            var ar = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            N = ar[0];
            M = ar[1];
            Ls = new List<short>[N];
            X = new short[N + 1, N];
            Mem = new int[N];
            for (int i = 0; i < N; i++)
            {
                Ls[i] = new List<short>();
            }
            for (int i = 0; i < M; i++)
            {
                var ar2 = Console.ReadLine().Split(' ').Select(z => short.Parse(z)).ToArray();
                Ls[ar2[1] - 1].Add((short)(ar2[0] - 1));
            }
            for (int i = 0; i < N; i++)
            {
                Ls[i].Sort();
            }
            for (int i = 0; i < N; i++)
            {
                Mem[i] = -1;
            }
            for (int j = 0; j < N; j++)
            {
                var i = 0;
                foreach (var k in Ls[j])
                {
                    for (; i <= k; i++)
                    {
                        X[i, j] = k;
                    }
                }
                for (; i <= j; i++)
                {
                    X[i, j] = short.MaxValue;
                }
            }
            for (int i = 0; i < N; i++)
            {
                var k = short.MaxValue;
                for (int j = N - 1; j >= i; j--)
                {
                    X[j + 1, i] = k;
                    k = Math.Min(k, X[i, j]);
                }
            }
            {
                var min = N;
                for (int j = N - 1; j >= 0; j--)
                {
                    if (Ls[j].Count > 0 && Ls[j][0] < min)
                    {
                        min = Ls[j][0];
                        if (min == 0)
                        {
                            break;
                        }
                    }
                    if (j == min - 1)
                    {
                        break;
                    }
                }
                for (int i = N - 1; i >= 0; i--)
                {
                    Solve(i);
                }
                Console.WriteLine(Solve(0) + (min == 0 ? 0 : 1));
            }
        }

        private int Solve(int i)
        {
            if (i >= N)
            {
                return 1;
            }
            if (Mem[i] == -1)
            {
                Mem[i] = Solve(i + 1);
                var max = i - 1;
                for (int j = i; j < N; j++)
                {
                    var k = X[i, j];
                    if (k > max + 1)
                    {
                        if (Ls[j].Count > 0 && Ls[j][0] < i)
                        {
                            k = X[j + 1, i];
                        }
                    }
                    if (k <= max + 1)
                    {
                        Mem[i] += Solve(j + 2);
                        Mem[i] %= Modulo;
                        max = j;
                    }
                }
            }
            return Mem[i];
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var t = int.Parse(Console.ReadLine());
            for (int i = 0; i < t; i++)
            {
                new Solver().Start();
            }
        }
    }
}
