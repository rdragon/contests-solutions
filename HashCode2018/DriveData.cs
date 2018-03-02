using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  class DriveData
  {
    public long Idle { get; }
    public long Reposition { get; }
    public long Pay { get; }
    public long Bonus { get; }

    public DriveData(Vehicle vehicle, Ride ride, Brain brain)
    {
      Reposition = vehicle.Pos.DistanceTo(ride.StartPos);
      var atStartTick = vehicle.TickFree + Reposition;
      var startingTick = Math.Max(atStartTick, ride.MinStart);
      Idle = startingTick - atStartTick;
      var atEndTick = startingTick + ride.Distance;
      Bonus = atStartTick <= ride.MinStart ? brain.Bonus : 0;
      Pay = atEndTick <= ride.MaxEnd ? ride.Distance : 0;
    }
  }
}
