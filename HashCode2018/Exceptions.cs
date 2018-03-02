using System;
using System.Collections.Generic;
using System.Text;

namespace HashCode2018
{
  public class ParseException : Exception
  {
    public ParseException(string message = null, Exception innerException = null) : base(message, innerException) { }
  }
}
