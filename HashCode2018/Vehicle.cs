using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  class Vehicle
  {
    public Vec Pos { get; set; } = new Vec();
    public int TickFree { get; set; }
    public Stack<RideScore> RideScores { get; set; }
    public List<Ride> Assignments { get; } = new List<Ride>();
  }
}
