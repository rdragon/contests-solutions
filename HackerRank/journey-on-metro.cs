// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JourneyOnMetro
{
    class Vertex
    {
        internal List<Edge> Edges = new List<Edge>();
    }

    class Edge
    {
        internal Vertex End;
        internal double Weight;
    }

    class Solver
    {
        int N, K;
        List<Vertex> Vertices=new List<Vertex>();
        internal void Start()
        {
            var ar = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            N = ar[0];
            K = ar[1];
            for (int i = 0; i < N; i++)
            {
                Vertices.Add(new Vertex());
            }
            for (int i = 0; i < N-1; i++)
            {
                var ar2 = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
                var u= Vertices[ar2[0] - 1];
                var v=Vertices[ar2[1] - 1];
                var weight=ar2[2];
                u.Edges.Add(new Edge { End = v, Weight = weight });
                v.Edges.Add(new Edge { End = u, Weight = weight });
            }

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
