using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace HashCode2018
{
  class Brain : ISolver
  {
    private int _minDistanceToMid = 3000;
    private static readonly double DistanceToMidMultiplier = 4;

    public int R { get; set; }
    public int C { get; set; }
    public int VehicleCount { get; set; }
    public int RideCount { get; set; }
    public int StepCount { get; set; }
    public int Bonus { get; set; }
    public Dictionary<int, Ride> Rides { get; } = new Dictionary<int, Ride>();
    public List<Vehicle> Vehicles { get; set; }
    public List<Ride> AllRides { get; } = new List<Ride>();
    public Vec Mid { get; set; }

    public void Solve(Reader reader, TextWriter writer)
    {
      R = reader.ReadInt();
      C = reader.ReadInt();
      VehicleCount = reader.ReadInt();
      RideCount = reader.ReadInt();
      Bonus = reader.ReadInt();
      StepCount = reader.ReadInt();
      for (int i = 0; i < RideCount; i++)
        Rides.Add(i, new Ride(reader, i));
      AllRides.AddRange(Rides.Values);
      Vehicles = new List<Vehicle>(VehicleCount);
      for (int i = 0; i < VehicleCount; i++)
        Vehicles.Add(new Vehicle());
      Mid = new Vec(AllRides.Sum(z => z.StartPos.X) / AllRides.Count, AllRides.Sum(z => z.StartPos.Y) / AllRides.Count);
      if (Path.GetFileNameWithoutExtension(reader.File) != "d_metropolis")
        _minDistanceToMid = int.MaxValue;
      Solve();
      WriteOutput(writer);
      WriteAnalytics(reader.File);
    }

    private void WriteOutput(TextWriter writer)
    {
      foreach (var vehicle in Vehicles)
      {
        var assignments = vehicle.Assignments;
        writer.Write(assignments.Count + " ");
        writer.Write(string.Join(" ", assignments.Select(z => z.Index)));
        writer.WriteLine();
      }
    }

    private void FirstTry()
    {
      var rides = Rides.Values.OrderBy(z => z.MinStart).ToList();
      var i = 0;
      foreach (var ride in rides)
        Vehicles[++i % VehicleCount].Assignments.Add(ride);
    }

    private void Solve()
    {
      var vehicles = Vehicles.ToList();
      foreach (var vehicle in vehicles)
        vehicle.RideScores = GetRideScores(vehicle);
      while (true)
      {
        // pop all ride scores for already assigned rides
        foreach (var vehicle in vehicles)
          while (vehicle.RideScores.Count > 0 && vehicle.RideScores.Peek().Ride.Assigned)
            vehicle.RideScores.Pop();

        // remove vehicles without any ride scores (they cannot increase our score)
        vehicles = vehicles.Where(z => z.RideScores.Count > 0).ToList();
        if (vehicles.Count == 0)
          return;

        var bestVehicle = vehicles.OrderBy(z => z.RideScores.Peek().Score).First();
        var bestRide = bestVehicle.RideScores.Pop().Ride;
        var (endPos, endTick, _) = Drive(bestVehicle, bestRide);
        bestVehicle.Assignments.Add(bestRide);
        bestVehicle.Pos = endPos;
        bestVehicle.TickFree = endTick;
        bestRide.Assigned = true;
        Rides.Remove(bestRide.Index);
        bestVehicle.RideScores = GetRideScores(bestVehicle);
      }
    }

    private Stack<RideScore> GetRideScores(Vehicle vehicle)
    {
      var rideScores = new List<RideScore>(Rides.Count);
      foreach (var ride in Rides.Values)
      {
        var (_, _, score) = Drive(vehicle, ride);
        if (0 < score)
          rideScores.Add(new RideScore(ride, score));
      }
      return new Stack<RideScore>(rideScores.OrderBy(z => z.Score));
    }

    private (Vec, int, double) Drive(Vehicle vehicle, Ride ride)
    {
      var repositionLength = vehicle.Pos.DistanceTo(ride.StartPos);
      var atStartTick = vehicle.TickFree + repositionLength;
      var startingTick = Math.Max(atStartTick, ride.MinStart);
      var idleLength = startingTick - atStartTick;
      var atEndTick = startingTick + ride.Distance;
      var points = (atStartTick <= ride.MinStart ? Bonus : 0) + (atEndTick <= ride.MaxEnd ? ride.Distance : 0);
      var toMid = ride.EndPos.DistanceTo(Mid);
      var cost = repositionLength + idleLength + ride.Distance + (toMid < _minDistanceToMid ? 0 : DistanceToMidMultiplier * toMid);
      var score = points / cost;
      return (ride.EndPos, atEndTick, score);
    }

    private void WriteAnalytics(string file)
    {
      var datas = new List<DriveData>();
      long leftOvers = 0;
      foreach (var vehicle in Vehicles)
      {
        vehicle.Pos = new Vec();
        vehicle.TickFree = 0;
        foreach (var ride in vehicle.Assignments)
        {
          datas.Add(new DriveData(vehicle, ride, this));
          var (endPos, endTick, _) = Drive(vehicle, ride);
          vehicle.Pos = endPos;
          vehicle.TickFree = endTick;
        }
        leftOvers += Math.Max(0, StepCount - vehicle.TickFree);
      }

      var paid = datas.Sum(z => z.Pay);
      var maxPaid = AllRides.Sum(z => z.Distance);
      var bonus = datas.Sum(z => z.Bonus);
      var maxBonus = AllRides.Where(z => z.StartPos.DistanceTo(new Vec()) <= z.MinStart).Count() * Bonus;
      var idle = datas.Sum(z => z.Idle);
      var unpaid = datas.Sum(z => z.Reposition);

      string f(long x) => g(x.ToString()).PadLeft(10, ' ');
      string g(string x) => x.Length <= 3 ? x : g(x.Substring(0, x.Length - 3)) + "," + x.Substring(x.Length - 3);
      var s = $"Summary of {Path.GetFileNameWithoutExtension(file)}";
      Console.WriteLine(s);
      Console.WriteLine(new string('-', s.Length));
      Console.WriteLine($"[Cars: {g(Vehicles.Count + "")}, Rides: {g(AllRides.Count + "")}, Steps: {g(StepCount + "")}]");
      Console.WriteLine($"Score:      {f(paid + bonus)} ({f(maxPaid + maxBonus - paid - bonus)} left)");
      Console.WriteLine($"Paid:       {f(paid)} ({f(maxPaid - paid)} left)");
      Console.WriteLine($"Bonus:      {f(bonus)} ({f(maxBonus - bonus)} left)");
      Console.WriteLine($"Idle:       {f(idle)}");
      Console.WriteLine($"Unpaid:     {f(unpaid)}");
      Console.WriteLine($"Leftovers:  {f(leftOvers)}");
      Console.WriteLine();
    }
  }
}
