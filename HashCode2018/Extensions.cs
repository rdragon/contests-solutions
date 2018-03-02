using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HashCode2018
{
  static class Extensions
  {
    public static IEnumerable<T> WhereNotNull<T>(this IEnumerable<T> it) => it?.Where(x => x != null);
    public static IEnumerable<TResult> SelectNotNull<T, TResult>(this IEnumerable<T> it, Func<T, TResult> selector) =>
        it?.Select(selector).WhereNotNull();
  }
}
