using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HashCode2018
{
  interface ISolver
  {
    void Solve(Reader reader, TextWriter writer);
  }
}
