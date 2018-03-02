using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace HashCode2018
{
  static class Helper
  {
    public static void ForbidNull(params object[] values)
    {
      if (values.Any(z => z == null))
        throw new ArgumentNullException();
    }
    public static void RequireExistsFile(string file)
    {
      ForbidNull(file);
      if (!File.Exists(file))
        throw new FileNotFoundException($"File '{file}' not found.", file);
    }
    public static void RequireExistsDir(string dir)
    {
      ForbidNull(dir);
      if (!Directory.Exists(dir))
        throw new DirectoryNotFoundException($"Directory '{dir}' not found.");
    }
    public static void RequireNonNegative(int number)
    {
      if (number < 0)
        throw new ArgumentOutOfRangeException(nameof(number), "Value should be non-negative.");
    }
  }
}
