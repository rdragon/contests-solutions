// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// we distinguish two cases: k < 100 and k >= 100
// in the first case we use brute force
// in the second case we use a lookup table to get the indices of all the cards that are multiples of k, 
// and then we order the indices to compute the number of divisor ranges

namespace DivisorRanges
{
    class Program
    {
        class Aggregator
        {
            internal long sum;
            internal int length;
            internal int previousIndex = -2;

            internal void AddDivisorRange()
            {
                sum += ((long)length + 1) * length / 2;
                length = 0;
            }
        }

        static void Main(string[] args)
        {
            var n = int.Parse(Console.ReadLine());
            var cards = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            var indices = new int[n + 1];
            for (int i = 0; i < n; i++)
            {
                indices[cards[i]] = i;
            }
            var answers = new long[n + 1];
            answers[1] = ((long)n + 1) * n / 2;
            var q = int.Parse(Console.ReadLine());
            for (int i = 0; i < q; i++)
            {
                var k = int.Parse(Console.ReadLine());
                if (answers[k] == 0)
                {
                    Aggregator aggregator;
                    if (k < 100)
                    {
                        aggregator = cards.Select(z => z % k == 0).Aggregate(new Aggregator(), aggregate);
                    }
                    else
                    {
                        var someIndices = Enumerable.Range(1, n).Select(z => z * k).TakeWhile(z => z <= n).Select(z => indices[z]).ToList();
                        someIndices.Sort();
                        aggregator = someIndices.Aggregate(new Aggregator(), aggregate);
                    }
                    aggregator.AddDivisorRange();
                    answers[k] = aggregator.sum;
                }
                Console.WriteLine(answers[k]);
            }
        }

        private static Aggregator aggregate(Aggregator aggregator, bool b)
        {
            if (b)
            {
                aggregator.length++;
            }
            else if (aggregator.length > 0)
            {
                aggregator.AddDivisorRange();
            }
            return aggregator;
        }

        private static Aggregator aggregate(Aggregator aggregator, int i)
        {
            if (aggregator.previousIndex != i - 1)
            {
                aggregator.AddDivisorRange();
            }
            aggregator.length++;
            aggregator.previousIndex = i;
            return aggregator;
        }
    }
}
