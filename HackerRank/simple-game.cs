// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HackerRank.SimpleGame
{
    class Solution
    {
        private static int Modulo = 1000000007;
        private static int N;
        private static int M;
        private static int K;
        private static int XorTop;
        private static int[] G;

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
            K = 3;
            Run();
        }

        private static void ComputeG3()
        {
            var bound = N * 4;
            for (int i = 1; i <= N; i++)
            {
                var mex = new bool[bound];
                for (int a = 0; i - 1 - a >= a; a++)
                {
                    var b = i - 1 - a;
                    mex[G[a] ^ G[b]] = true;
                }
                for (int a = 0; i - 2 - a >= a; a++)
                {
                    var b = i - 2 - a;
                    mex[G[a] ^ G[b]] = true;
                }
                for (int a = 0; a < i - 2 - 2; a++)
                {
                    for (int b = a; b < i - 2 - 1 - a; b++)
                    {
                        var c = i - 2 - a - b;
                        mex[G[a] ^ G[b] ^ G[c]] = true;
                    }
                }
                for (int a = 0; a < bound; a++)
                {
                    if (mex[a] == false)
                    {
                        G[i] = a;
                        break;
                    }
                    if (a == bound - 1)
                    {
                        throw new Exception();
                    }
                }
            }
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

            G = new int[N + 1];
            if (K > 3)
            {
                for (int i = 0; i < N + 1; i++)
                {
                    G[i] = i;
                }
            }
            else
            {
                ComputeG3();
            }

            if (M == 1)
            {
                Console.WriteLine(G[N] == 0 ? 0 : 1);
                return;
            }

            var max = G.Max();
            XorTop = 1;
            while (XorTop <= max)
            {
                XorTop *= 2;
            }
            var m1 = (M + 1) / 2;
            var v = new int[GetIndex(m1 + 1, 0, 0)]; // number of ways to reach given position
            for (int pileSize = 0; pileSize <= N; pileSize++)
            {
                var i = GetIndex(1, pileSize, G[pileSize]);
                v[i] = 1;
            }
            for (int pileCount = 2; pileCount <= m1; pileCount++)
            {
                for (int x = 0; x < XorTop; x++)
                {
                    for (int sum = 0; sum <= N; sum++)
                    {
                        var index = GetIndex(pileCount - 1, sum, x);
                        if (v[index] > 0)
                        {
                            for (int pileSize = 0; pileSize + sum <= N; pileSize++)
                            {
                                var x1 = x ^ G[pileSize];
                                var index1 = GetIndex(pileCount, sum + pileSize, x1);
                                v[index1] = (v[index1] + v[index]) % Modulo;
                            }
                        }
                    }
                }
            }

            // meet
            var i1 = M / 2;
            var i2 = (M + 1) / 2;
            var loseCount = 0;
            for (int sum = 0; sum <= N; sum++)
            {
                for (int x = 0; x < XorTop; x++)
                {
                    var i = GetIndex(i1, sum, x);
                    var j = GetIndex(i2, N - sum, x);
                    loseCount = (loseCount + Product(v[i], v[j])) % Modulo;
                }
            }

            var total = GetTotal();
            //Console.WriteLine("total: " + total);
            Console.WriteLine((total + Modulo - loseCount) % Modulo);
        }

        private static int GetTotal()
        {
            var m = M;
            var n = N;
            var jump = n + 1;
            var v = new int[jump * (m - 1)];
            for (int m1 = 2; m1 <= m; m1++)
            {
                for (int x = 0; x <= n; x++)
                {
                    var tot = 0;
                    if (m1 == 2)
                    {
                        tot = x + 1;
                    }
                    else
                    {
                        for (int i = 0; i <= x; i++)
                        {
                            tot = (tot + v[jump * (m1 - 3) + i]) % Modulo;
                        }
                    }
                    v[jump * (m1 - 2) + x] = tot;
                }
            }
            return v[jump * (m - 2) + n];
        }

        private static int Product(int a, int b)
        {
            long la = a;
            long lb = b;
            var ans = (la * lb) % Modulo;
            return (int)ans;
        }

        private static int GetIndex(int pileCount, int sum, int xor)
        {
            return (pileCount - 1) * (XorTop * (N + 1)) + sum * XorTop + xor;
        }
    }
}
