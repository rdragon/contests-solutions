// 2016
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HackerRank.Temp
{
    class Solution
    {
#if !LOCAL
        public static void Main()
        {
            new Brain().Run(new Reader());
        }
#endif
    }

    class Brain
    {
        private Dictionary<long, long> Dict = new Dictionary<long, long>();
        private int N;
        private bool[] S;
        private bool[] T;

        public void Run(Reader r)
        {
            S = r.ReadString().Select(z => z == 'a').ToArray();
            N = S.Length;
            T = new bool[N];
            var ans = Calculate(0, 0, 0, 0);
            Console.WriteLine(ans);
        }

        private long Calculate(int s0, int t0, int t1, long tKey)
        {
            if (s0 == N)
            {
                return t1 == t0 ? 1 : 0;
            }
            if (!IsPossible(s0, t0, t1))
            {
                return 0;
            }
            long key = (uint)s0 | ((uint)(t1 - t0) << 6) | (tKey << 11);
            long z;
            if (Dict.TryGetValue(key, out z))
            {
                return z;
            }

            long tKey1 = (S[s0] ? ((uint)1 << (t1 - t0)) : 0) | tKey;
            if (t1 - t0 == 0)
            {
                T[t1] = S[s0];
                z = 2 * Calculate(s0 + 1, t0, t1 + 1, tKey1);
            }
            else
            {
                long x;
                if (S[s0] == T[t0])
                {
                    x = Calculate(s0 + 1, t0 + 1, t1, tKey >> 1);
                }
                else
                {
                    x = 0;
                }
                T[t1] = S[s0];
                z = x + Calculate(s0 + 1, t0, t1 + 1, tKey1);
            }
            Dict[key] = z;
            return z;
        }

        private bool IsPossible(int s0, int t0, int t1)
        {
            var t = t0;
            bool a = true;
            bool b = true;
            for (int s = s0; s < N; s++)
            {
                if (t < t1 && S[s] == T[t])
                {
                    t++;
                }
                else
                {
                    if (S[s])
                    {
                        a = !a;
                    }
                    else
                    {
                        b = !b;
                    }
                }
            }
            return t == t1 && a && b;
        }
    }

    class Reader
    {
        private Queue<string> Words;

        public Reader()
        {
            Words = new Queue<string>(Console.In.ReadToEnd().Split(new char[] { ' ', '\n' }));
        }

        public Reader(string path)
        {
            Words = new Queue<string>(File.ReadAllText(path).Split(new char[] { ' ', '\n' }));
        }

        public int ReadInt()
        {
            return int.Parse(Words.Dequeue());
        }

        public int[] ReadInts(int count)
        {
            var table = new int[count];
            for (int i = 0; i < count; i++)
            {
                table[i] = ReadInt();
            }
            return table;
        }

        public string ReadString()
        {
            return Words.Dequeue().Trim();
        }

        public void MoveNext()
        {
            Words.Dequeue();
        }
    }
}
