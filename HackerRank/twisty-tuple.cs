// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TwistyTuple
{
    class Program
    {
        static void Main(string[] args)
        {
            var n = int.Parse(Console.ReadLine());
            var ar = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            long sum = 0;
            for (int i = 0; i < n - 2; i++)
            {
                var smaller = 0;
                for (int j = i + 1; j < n; j++)
                {
                    if (ar[j] < ar[i])
                    {
                        smaller++;
                    }
                }
                for (int j = i + 1; j < n - 1; j++)
                {
                    if (ar[j] < ar[i])
                    {
                        smaller--;
                    }
                    else if (ar[j] > ar[i])
                    {
                        sum += smaller;
                    }
                }
            }
            Console.WriteLine(sum);
        }
    }
}
