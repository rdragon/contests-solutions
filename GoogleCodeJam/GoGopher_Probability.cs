// 2018-04-07
// go gopher, code jam 2018, qualification round

// this is not the solution, but it computes the probability that you solve the hidden test case using the algorithm in the other file
// for this we use markov chains

using System;
using System.Collections.Generic;
using System.Linq;

namespace CodeJam2018
{
    class GoGopher_Probability
    {
        static void Main() => Run();

        static void Run()
        {
            var n = 1000;
            var a = GetMatrix();
            var b = GetIdentity();
            var t = 1;
            while (true)
            {
                if ((n & t) == t)
                    b = Multiply(a, b);
                t = t * 2;
                if (n < t)
                    break;
                a = Multiply(a, a);
            }
            var v = GetVector();
            v = Multiply(b, v);
            var x = v.Rows[0][0];
            x = Math.Pow(x, 20);
            Console.WriteLine($"Probability that you solve the hidden test case: {x}");
        }

        static Matrix GetVector()
        {
            var v = new Matrix(208, 1);
            v.Rows[207][0] = 1;
            return v;
        }

        static Matrix GetIdentity()
        {
            var a = new Matrix(208, 208);
            for (int i = 0; i < 208; i++)
            {
                a.Rows[i][i] = 1;
            }
            return a;
        }

        static Matrix GetMatrix()
        {
            var a = new Matrix(208, 208);
            var i = 207;
            for (int t = 0; t < 23; t++)
            {
                for (int left = 9; 1 <= left; left--)
                {
                    a.Rows[i][i] = (9.0 - left) / 9;
                    if (0 < i)
                        a.Rows[i - 1][i] = left / 9.0;
                    i--;
                }
                a.Rows[0][0] = 1;
            }
            return a;
        }

        static Matrix Multiply(Matrix a, Matrix b)
        {
            var c = new Matrix(a.N, b.M);
            for (int i = 0; i < c.N; i++)
            {
                for (int j = 0; j < c.M; j++)
                {
                    var sum = 0.0;
                    for (int k = 0; k < a.M; k++)
                        sum += a.Rows[i][k] * b.Rows[k][j];
                    c.Rows[i][j] = sum;
                }
            }
            return c;
        }

        class Matrix
        {
            public List<List<double>> Rows { get; set; }

            public int N { get { return Rows.Count; } }
            public int M { get { return Rows[0].Count; } }

            public Matrix(int n, int m)
            {
                Rows = new List<List<double>>();
                for (int i = 0; i < n; i++)
                {
                    Rows.Add(Enumerable.Repeat<double>(0, m).ToList());
                }
            }
        }
    }
}
