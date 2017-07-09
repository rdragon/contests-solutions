// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HackerRank.DigitsSquareBoard
{
    class Solution
    {
        public static void Main2()
        {
            var t = int.Parse(Console.ReadLine());
            for (int i = 0; i < t; i++)
            {
                Run();
            }
        }

        private static void Run()
        {
            var n = int.Parse(Console.ReadLine());
            var ds = new int[n + 1, n + 1];
            for (int i = 0; i < n; i++)
            {
                var x = Console.ReadLine().Split(' ');
                var z = 0;
                for (int j = 0; j < n; j++)
                {
                    var y = int.Parse(x[j]);
                    y = (y == 1 || y == 4 || y == 6 || y == 8 || y == 9) ? 1 : 0;
                    z += y;
                    y = z;
                    if (i > 0)
                    {
                        y += ds[i, j + 1];
                    }
                    ds[i + 1, j + 1] = y;
                }
            }
            Func<int, int, int, int, int> getIndex = (x, y, w, h) =>
               {
                   return y + x * n + (h - 1) * n * n + (w - 1) * n * n * n;
               };

            var mem = new int[n * n * n * n];
            for (int w = 1; w <= n; w++)
            {
                for (int h = 1; h <= n; h++)
                {
                    if (w == 1 && h == 1)
                    {
                        continue;
                    }
                    for (int x = 0; x <= n - w; x++)
                    {
                        for (int y = 0; y <= n - h; y++)
                        {
                            var x2 = x + w - 1;
                            var y2 = y + h - 1;
                            var count = ds[y2 + 1, x2 + 1] + ds[y, x] - ds[y, x2 + 1] - ds[y2 + 1, x];
                            if (count > 0)
                            {
                                var zs = new bool[w - 1 + h];
                                for (int w1 = 1; w1 < w; w1++)
                                {
                                    var a = mem[getIndex(x, y, w1, h)] ^ mem[getIndex(x + w1, y, w - w1, h)];
                                    if (a < zs.Length)
                                    {
                                        zs[a] = true;
                                    }
                                }
                                for (int h1 = 1; h1 < h; h1++)
                                {
                                    var a = mem[getIndex(x, y, w, h1)] ^ mem[getIndex(x, y + h1, w, h - h1)];
                                    if (a < zs.Length)
                                    {
                                        zs[a] = true;
                                    }
                                }
                                int b = 0;
                                while (zs[b])
                                {
                                    b++;
                                }
                                mem[getIndex(x, y, w, h)] = b;
                            }
                        }
                    }
                }
            }
            if (mem[getIndex(0, 0, n, n)] == 0)
            {
                Console.WriteLine("Second");
            }
            else
            {
                Console.WriteLine("First");
            }
        }
    }
}
