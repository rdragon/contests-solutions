// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TotientUntilTheEnd
{
    class Block
    {
        internal int bottom;
        internal int top;
    }

    class Solver
    {
        internal int Iterations;
        static int[] SmallPrimes = Program.GetPrimes((int)Math.Sqrt(Program.UpperLimit) + 1).ToArray();
        static Dictionary<int, int[]> Factorizations = new Dictionary<int, int[]>();
        SortedDictionary<int, List<Block>> BlockCollection = new SortedDictionary<int, List<Block>>();

        internal void HandleInput(int a, int b)
        {
            foreach (var p in GetPrimeFactors(a))
            {
                AddBlock(p, 0, b - 1);
            }
        }

        private void AddBlock(int p, int bottom, int top)
        {
            var key = -p;
            if (!BlockCollection.ContainsKey(key))
            {
                BlockCollection[key] = new List<Block>();
            }
            var blocks = BlockCollection[key];
            var n = top - bottom + 1;
            var inserted = false;
            for (int i = 0; i < blocks.Count; i++)
            {
                var block = blocks[i];
                if (inserted)
                {
                    var prev = blocks[i - 1];
                    if (block.bottom <= prev.top + 1)
                    {
                        prev.top += block.top - block.bottom + 1;
                        blocks.RemoveAt(i);
                        i--;
                    }
                    else
                    {
                        break;
                    }
                }
                else if (block.bottom > top + 1)
                {
                    blocks.Insert(i, new Block { bottom = bottom, top = top });
                    inserted = true;
                    break;
                }
                else if (block.top < bottom - 1)
                {
                    continue;
                }
                else if (bottom >= block.bottom)
                {
                    block.top += n;
                    inserted = true;
                }
                else
                {
                    var len = block.top - block.bottom + 1 + n;
                    block.bottom = bottom;
                    block.top = bottom + len - 1;
                    inserted = true;
                }
            }
            if (!inserted)
            {
                blocks.Add(new Block { bottom = bottom, top = top });
            }
        }

        private IEnumerable<int> GetPrimeFactors(int a)
        {
            if (!Factorizations.ContainsKey(a))
            {
                Factorizations[a] = IteratePrimeFactors(a).ToArray();
            }
            return Factorizations[a];
        }

        private IEnumerable<int> IteratePrimeFactors(int a)
        {
            foreach (var p in SmallPrimes.TakeWhile(z => z <= Math.Sqrt(a)))
            {
                while (a % p == 0)
                {
                    a /= p;
                    yield return p;
                    if (a == 1)
                    {
                        break;
                    }
                }
            }
            if (a > 1)
            {
                yield return a;
            }
        }

        internal void ApplyTotientUntilTheEnd()
        {
            while (BlockCollection.Any())
            {
                var pair = BlockCollection.First();
                var p = -pair.Key;
                var blocks = pair.Value;
                BlockCollection.Remove(pair.Key);

                if (p == 2)
                {
                    if (blocks.Count > 0)
                    {
                        Iterations = blocks[blocks.Count - 1].top + 1;
                    }
                }
                else
                {
                    foreach (var q in GetPrimeFactors(p - 1))
                    {
                        foreach (var block in blocks)
                        {
                            AddBlock(q, block.bottom + 1, block.top + 1);
                        }
                    }
                }
            }
        }
    }

    class Program
    {
        internal const int UpperLimit = 2000000;

        static void Main(string[] args)
        {

            var t = int.Parse(Console.ReadLine());
            for (int i = 0; i < t; i++)
            {
                var values = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
                var solver = new Solver();
                for (int j = 0; j < 4; j++)
                {
                    solver.HandleInput(values[j * 2], values[j * 2 + 1]);
                }
                solver.ApplyTotientUntilTheEnd();
                Console.WriteLine(solver.Iterations);
            }
        }

        internal static IEnumerable<int> GetPrimes(int upperLimit)
        {
            var sieveLength = upperLimit / 2;
            bool[] sieve = new bool[sieveLength];
            bool doneSieving = false;

            if (upperLimit <= 2)
                yield break;

            yield return 2;

            for (int i = 0, j = 3; j < upperLimit; i++, j += 2)
            {
                if (!sieve[i])
                {
                    yield return j;
                    if (!doneSieving)
                    {
                        int k = j * j / 2 - 1;
                        if (k >= sieveLength)
                            doneSieving = true;
                        for (; k < sieveLength; k += j)
                            sieve[k] = true;
                    }
                }
            }
        }
    }
}