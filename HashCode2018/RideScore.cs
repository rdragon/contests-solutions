using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  class RideScore
  {
    public Ride Ride { get; }
    public double Score { get; }

    public RideScore(Ride ride, double score)
    {
      Ride = ride;
      Score = score;
    }
  }
}
