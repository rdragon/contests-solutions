using System;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

namespace HashCode2018
{
  static class Program
  {
    private static int Main(string[] args)
    {
      try
      {
        if (1 != args.Length)
          throw new ArgumentException($"Expecting a single argument but got {args.Length} arguments");
        SolveDirectory(args[0]);
        return 0;
      }
      catch (Exception ex)
      {
        Console.Error.WriteLine("An error has occurred");
        Console.Error.WriteLine(ex);
        return 1;
      }
    }

    private static ISolver GetSolver()
    {
      return new Brain();
    }

    private static void SolveDirectory(string dir)
    {
      Helper.RequireExistsDir(dir);
      foreach (var outFile in Directory.GetFiles(dir, "*.out"))
        File.Delete(outFile);
      foreach (var inFile in Directory.GetFiles(dir, "*.in"))
      {
        var sw = new Stopwatch();
        var outFile = Regex.Replace(inFile, @"\.in$", ".out");
        var reader = Reader.FromFile(inFile);
        using (var stream = File.Open(outFile, FileMode.Create, FileAccess.Write))
        using (var writer = new StreamWriter(stream))
        {
          var solver = GetSolver();
          Console.WriteLine($"Processing file '{inFile}'...");
          Console.Out.Flush();
          sw.Start();
          solver.Solve(reader, writer);
          sw.Stop();
        }
        Console.WriteLine($"Done (took {sw.ElapsedMilliseconds} ms)");
      }
    }
  }
}
