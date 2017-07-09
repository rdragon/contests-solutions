// 2016
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
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
        public void Run(Reader r)
        {
            var t = r.ReadInt();
            for (int i = 0; i < t; i++)
            {
                var n = r.ReadInt();
                var hps = r.ReadInts(n).ToList();
                hps.Sort();
                var sums = new BigInteger[n + 1];
                for (int j = n - 1; j >= 0; j--)
                {
                    sums[j] = sums[j + 1] + hps[j];
                }
                BigInteger total = 0;
                BigInteger s = 1;
                for (int j = 0; j < n; j++)
                {
                    var score = hps[j] * s;
                    if (score < sums[j + 1])
                    {
                        s++;
                    }
                    else
                    {
                        total += score;
                    }
                }
                Console.WriteLine(total);
            }
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

        public void MoveNext()
        {
            Words.Dequeue();
        }
    }
}
