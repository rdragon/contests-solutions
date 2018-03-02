using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  class Ride
  {
    public int Index { get; }
    public Vec StartPos { get; }
    public Vec EndPos { get; }
    public int MinStart { get; }
    public int MaxEnd { get; }
    public bool Assigned { get; set; }

    public Ride(Reader r, int index)
    {
      Index = index;
      StartPos = new Vec(r);
      EndPos = new Vec(r);
      MinStart = r.ReadInt();
      MaxEnd = r.ReadInt();
    }

    public int Distance => EndPos.DistanceTo(StartPos);
  }
}
