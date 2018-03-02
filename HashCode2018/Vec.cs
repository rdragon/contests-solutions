using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  class Vec
  {
    public int X { get; }
    public int Y { get; }

    public Vec() { }

    public Vec(int x, int y)
    {
      X = x;
      Y = y;
    }

    public Vec(Reader r)
    {
      X = r.ReadInt();
      Y = r.ReadInt();
    }
    public int DistanceTo(Vec other)
    {
      return Math.Abs(X - other.X) + Math.Abs(Y - other.Y);
    }
  }
}
