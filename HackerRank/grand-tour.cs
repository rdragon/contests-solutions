// 2016
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrandTour
{
    class Solver
    {
        internal int N;
        internal int[] SightSeeing;
        internal long Sum;
        internal List<Vertex> AllVertices = new List<Vertex>();
        internal List<Vertex> Vertices;
        internal int TotalCycleTime;

        internal void Start()
        {
            N = int.Parse(Console.ReadLine());
            SightSeeing = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
            for (int i = 0; i < N; i++)
            {
                AllVertices.Add(new Vertex());
            }
            ReadEdges();
            Vertices = new List<Vertex>(AllVertices);
            MakeCycleGraph();
            ComputeCycleTime();
            Console.ReadLine();
            ComputeTravelTime();
        }

        private void ComputeTravelTime()
        {
            Sum = 0;
            Vertex prev = null;
            foreach (var city in Console.ReadLine().Split(' ').Select(z => int.Parse(z)))
            {
                var vertex = AllVertices[city - 1];
                if (prev != null)
                {
                    Sum += GetTime(prev, vertex);
                }
                Sum += SightSeeing[city - 1];
                prev = vertex;
            }
        }

        private void ComputeCycleTime()
        {
            if (Vertices.Count > 1)
            {
                var start = Vertices[0];
                start.Time = 0;
                var vertex = start;
                var edge = start.Edges[0];
                while (true)
                {
                    TotalCycleTime += edge.Weight;
                    vertex = edge.GetOtherEnd(vertex);
                    if (vertex == start)
                    {
                        break;
                    }
                    vertex.Time = TotalCycleTime;
                    edge = vertex.GetOtherEdge(edge);
                }
            }
        }

        private void MakeCycleGraph()
        {
            var leaves = new List<Vertex>(Vertices.Where(z => z.Degree == 1));
            int generation = 1;
            while (leaves.Count > 0)
            {
                var nextLeaves = new List<Vertex>();
                foreach (var leave in leaves)
                {
                    var street = new Street { Generation = generation };
                    var vertex = leave;
                    var edge = vertex.Edges[0];
                    while (true)
                    {
                        edge.SetEnd(vertex);
                        street.Edges.Insert(0, edge);
                        vertex.Street = street;
                        vertex = edge.Start;
                        if (vertex.Degree != 2 || vertex.Generation == generation)
                        {
                            break;
                        }
                        edge = vertex.GetOtherEdge(edge);
                    }
                    vertex.Edges.Remove(edge);
                    vertex.Generation = generation;
                    var sum = 0;
                    foreach (var e in street.Edges)
                    {
                        sum += e.Weight;
                        e.End.Time = sum;
                    }
                    if (vertex.Degree == 1)
                    {
                        nextLeaves.Add(vertex);
                    }
                    else if (vertex.Degree == 0)
                    {
                        nextLeaves.Clear();
                        break;
                    }
                }
                Vertices = new List<Vertex>(Vertices.Where(z => z.Street == null));
                leaves = nextLeaves;
                generation++;
            }
        }

        private void ReadEdges()
        {
            for (int i = 0; i < N; i++)
            {
                var ar = Console.ReadLine().Split(' ').Select(z => int.Parse(z)).ToArray();
                var edge = new Edge { Start = AllVertices[ar[0] - 1], End = AllVertices[ar[1] - 1], Weight = ar[2] };
                if (edge.Start != edge.End)
                {
                    var duplicateEdge = edge.Start.Edges.Where(z => z.Start == edge.End || z.End == edge.End).FirstOrDefault();
                    if (duplicateEdge != null)
                    {
                        duplicateEdge.Weight = Math.Min(duplicateEdge.Weight, edge.Weight);
                    }
                    else
                    {
                        edge.Start.Edges.Add(edge);
                        edge.End.Edges.Add(edge);
                    }
                }
            }
        }

        private int GetTime(Vertex start, Vertex end)
        {
            if (start == end)
            {
                return 0;
            }
            else if (start.Street == null && end.Street == null)
            {
                return GetCycleTime(start, end);
            }
            else if (start.Street == end.Street)
            {
                return Math.Abs(start.Time - end.Time);
            }
            else if (start.Street == null || (end.Street != null && start.Street.Generation > end.Street.Generation))
            {
                var swap = start;
                start = end;
                end = swap;
            }
            return start.Time + GetTime(start.Street.Vertex, end);
        }

        private int GetCycleTime(Vertex start, Vertex end)
        {
            var time = Math.Abs(start.Time - end.Time);
            return Math.Min(time, TotalCycleTime - time);
        }
    }

    class Vertex
    {
        internal List<Edge> Edges = new List<Edge>();
        internal Street Street;
        internal int Generation;
        internal int Time;
        internal int Degree { get { return Edges.Count; } }

        internal Edge GetOtherEdge(Edge edge)
        {
            return Edges[0] == edge ? Edges[1] : Edges[0];
        }
    }

    class Edge
    {
        internal Vertex Start;
        internal Vertex End;
        internal int Weight;

        internal void SetEnd(Vertex end)
        {
            if (End != end)
            {
                var swap = Start;
                Start = End;
                End = swap;
            }
        }

        internal Vertex GetOtherEnd(Vertex start)
        {
            return start == Start ? End : Start;
        }
    }

    class Street
    {
        internal List<Edge> Edges = new List<Edge>();
        internal int Generation;
        internal Vertex Vertex { get { return Edges[0].Start; } }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var solver = new Solver();
            solver.Start();
            Console.WriteLine(solver.Sum);
        }
    }
}
