// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HackerRank.SimpleGame2
{
    /// <summary>
    /// fast algorithm for K > 3
    /// </summary>
    class Solution
    {
        private static int Modulo = 1000000007;
        private static int N;
        private static int M;
        private static int K;
        private static int[] ChooseTable;

        public static void Main2()
        {
            var nmk = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            M = nmk[1];
            N = nmk[0] - M;
            K = nmk[2];

            Run();
        }

        public static void Main3()
        {
            N = 600;
            M = 10;
            K = 4;

            Run();
        }

        private static void Run()
        {
            if (K == 2)
            {
                if (N % 2 == 1)
                {
                    Console.WriteLine(GetTotal());
                }
                else
                {
                    Console.WriteLine(0);
                }
                return;
            }

            // k == 3 is not supported
            if (K == 3)
            {
                throw new Exception("not supported");
            }

            if (M == 1)
            {
                Console.WriteLine(N > 0 ? 1 : 0);
                return;
            }

            CreateChooseTable();
            var v = new int[N + 1]; // v: number of ways to reach a specific number of stones left
            for (int j = 0; j <= M && j <= N; j += 2) // j: number of odd pile sizes (number of ones at position 0 in binary representation of pile sizes)
            {
                v[N - j] = Choose(j);
            }
            var factor = 2; // number of stones required for each one at current position in binary representation
            int loseCount = v[0];
            while (factor <= N)
            {
                var w = new int[N + 1];
                for (int a = 1; a <= N; a++) // a: stones left
                {
                    if (v[a] > 0)
                    {
                        for (int j = 0; j <= M && factor * j <= a; j += 2) // j: number of ones in binary representations of pile sizes at current position
                        {
                            var a1 = a - factor * j;
                            var count1 = Multiply(v[a], Choose(j));
                            w[a1] = (w[a1] + count1) % Modulo;
                        }
                    }
                }
                factor *= 2;
                v = w;
                loseCount = (loseCount + v[0]) % Modulo;
            }
            var ans = (GetTotal() - loseCount + Modulo) % Modulo;
            Console.WriteLine(ans);
        }

        private static void CreateChooseTable()
        {
            ChooseTable = new int[M + 1];
            for (int i = 0; i <= M; i++)
            {
                ChooseTable[i] = Multiply(Factorial(M), Inverse(Multiply(Factorial(i), Factorial(M - i))));
            }
        }

        private static int Choose(int i)
        {
            return ChooseTable[i];
        }

        private static int Factorial(int x)
        {
            long product = 1;
            for (long i = 2; i <= x; i++)
            {
                product = (i * product) % Modulo;
            }
            return (int)product;
        }

        private static int Multiply(int a, int b)
        {
            long x = a;
            long y = b;
            long z = (x * y) % Modulo;
            return (int)z;
        }

        private static int Inverse(int x)
        {
            if (x == 0)
            {
                throw new Exception();
            }
            return Pow(x, Modulo - 2);
        }

        private static int Pow(int x, int a)
        {
            long y = x;
            long ans = 1;
            var b = a;
            while (b > 0)
            {
                if (b % 2 == 1)
                {
                    ans = (ans * y) % Modulo;
                }
                y = (y * y) % Modulo;
                b /= 2;
            }
            return (int)ans;
        }

        private static int GetTotal()
        {
            if (M == 1 || N == 0)
            {
                return 1;
            }
            var jump = N + 1;
            var v = new int[jump * (M + 1)];
            for (int i = 2; i <= M; i++)
            {
                for (int a = i == M ? N : 0; a <= N; a++)
                {
                    var count = 0;
                    if (i == 2)
                    {
                        count = a + 1;
                    }
                    else
                    {
                        for (int j = 0; j <= a; j++)
                        {
                            count = (count + v[jump * (i - 1) + j]) % Modulo;
                        }
                    }
                    v[jump * i + a] = count;
                }
            }
            return v[jump * M + N];
        }
    }
}
