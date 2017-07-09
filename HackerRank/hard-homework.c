// 2016-12-05
// hard-homework, week of code 26
// not a valid solution (score 44.2 / 67.5)

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define min_(x, y) ((y) < (x) ? (y) : (x))
#define max_(x, y) ((y) > (x) ? (y) : (x))

typedef struct A
{
  double x;
  int t;
} A;

struct A ar[3000001];

time_t start;

int timeLeft()
{
  return (double)(clock() - start) / CLOCKS_PER_SEC < 1.8;
}

int compar(const void *a, const void *b)
{
  return ((A *)a)->x < ((A *)b)->x ? 1 : -1;
}

int main()
{
  start = clock();
  int N;
  scanf("%d", &N);
  int n = N - 2;
  for (int t = 1; t <= n; ++t)
  {
    ar[t - 1] = (A) {sin(t), t};
  }
  qsort(ar, n, sizeof(struct A), compar);
  double best = 0;
  int m = min_(n, 3000);
  for (int i = 0; i < m; ++i)
  {
    double x = ar[i].x;
    int k = N - ar[i].t;
    for (int j = i; j < m; ++j)
    {
      int l = k - ar[j].t;
      if (l > 0)
      {
        double y = x + ar[j].x + sin(l);
        if (y > best)
        {
          best = y;
        }
      }
    }
  }
  while (timeLeft() && m < n)
  {
    int p = m;
    m = min_(n, m + 300);
    for (int i = 0; i < m; ++i)
    {
      double x = ar[i].x;
      int k = N - ar[i].t;
      for (int j = max_(i, p); j < m; ++j)
      {
        int l = k - ar[j].t;
        if (l > 0)
        {
          double y = x + ar[j].x + sin(l);
          if (y > best)
          {
            best = y;
          }
        }
      }
    }
  }
  printf("%.9lf\n", best);
  return 0;
}