// 2016-11-30
// twins, week of code 26
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef long long ll;

char xs[100000];
char ys[1000000];

int main()
{
  ll n, m;
  scanf("%lld%lld", &n, &m);
  ll a = (ll)ceil(sqrt(m)) / 2 + 2;
  for (ll i = 0; i < a; ++i)
    xs[i] = 1;
  ll b = (m - n) / 2 + 2;
  for (ll i = 0; i < b; ++i)
    ys[i] = 1;
  ll y0 = n + !(n % 2);
  if (y0 == 1)
    y0 = 3;
  ll x = 3;
  ll y;
  for (ll i = 0; i < a; ++i, x += 2)
  {
    if (xs[i])
    {
      for (ll j = (x * x - 3) / 2; j < a; j += x)
        xs[j] = 0;
      y = y0 + (x - y0 % x) % x;
      if (y % 2 == 0)
        y += x;
      if (y < x * x)
        y = x * x;
      for (ll j = (y - y0) / 2; j < b; j += x)
        ys[j] = 0;
    }
  }
  y = y0;
  ll count = 0;
  for (ll i = 0; y <= m - 2; ++i, y += 2)
  {
    if (ys[i] && ys[i + 1])
    {
      count++;
    }
  }
  printf("%lld\n", count);
  return 0;
}