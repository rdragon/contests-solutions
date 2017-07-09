// 2016-12-19
// sam's-puzzle, world codesprint 8
// gets 61% of possible points
#ifndef LCL
#define NDEBUG
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <limits.h>
#include <time.h>

#define swap_(x, y) { int z = x; x = y; y = z; }
#define min_(x, y) ((y) < (x) ? (y) : (x))
#define max_(x, y) ((y) > (x) ? (y) : (x))

int n;
int nn; // n * n
int a0[900];
int a[900];
clock_t stop_time;
int moves[500 * 3];
int best_moves[500 * 3];
int movec;
int best_movec;
int best_score;
int t;
int ti;
int tj;
int di; // destination i
int dj;
int dsum;

int timeLeft()
{
  return clock() < stop_time;
}

void print()
{
  for (int i = 0; i < n; ++i)
  {
    for (int j = 0; j < n; ++j)
    {
      printf("%3d ", a[i * n + j]);
    }
    printf("\n");
  }
  printf("\n");
}

void copy(int *x, int *y)
{
  for (int i = 0; i < nn; ++i)
  {
    x[i] = y[i];
  }
}

int get_score()
{
  int count = 0;
  for (int i = 0; i < n - 1; ++i)
  {
    for (int j = 0; j < n - 1; ++j)
    {
      int x = a[i * n + j];
      for (int k = i + 1; k < n; ++k)
      {
        if (a[k * n + j] > x)
        {
          count++;
        }
      }
      for (int k = j + 1; k < n; ++k)
      {
        if (a[i * n + k] > x)
        {
          count++;
        }
      }
    }
  }
  return count;
}

void rot(int i, int j, int k)
{
  int kk = k * k;
  static int b[900];
  int p = 0;
  for (int i1 = i; i1 < i + k; ++i1)
  {
    for (int j1 = j; j1 < j + k; ++j1)
    {
      b[p++] = a[i1 * n + j1];
    }
  }
  int j2 = 0;
  for (int i1 = i; i1 < i + k; ++i1)
  {
    int i2 = k - 1;
    for (int j1 = j; j1 < j + k; ++j1)
    {
      a[i1 * n + j1] = b[i2 * k + j2];
      i2--;
    }
    j2++;
  }
}

void addMove(int i, int j, int k)
{
  assert(i >= 0 && j >= 0 && i + k <= n && j + k <= n);
  rot(i, j, k);
  assert(movec < 500);
  moves[movec * 3] = i;
  moves[movec * 3 + 1] = j;
  moves[movec * 3 + 2] = k;
  movec++;
}

int move1();

int moveLeft(int l)
{
  addMove(ti - l, tj - l, l + 1);
  tj -= l;
  return move1();
}

int moveRight(int l)
{
  addMove(ti, tj, l + 1);
  tj += l;
  return move1();
}

int moveUp(int l)
{
  addMove(ti - l, tj, l + 1);
  ti -= l;
  return move1();
}

int moveDown(int l)
{
  addMove(ti, tj - l, l + 1);
  ti += l;
  return move1();
}

int move1()
{
  assert(a[ti * n + tj] == t);
  if (ti == di && tj == dj)
  {
    return 1;
  }
  if (movec == 500)
  {
    return 0;
  }
  int k = min_(n - di, n - dj);
  int si = ti - di;
  int sj = tj - dj;
  if (tj == dj && si < k)
  {
    assert(si >= 0);
    return moveUp(si);
  }
  if (si >= 0 && sj >= 0)
  {
    // if inside square
    if (si < k && sj < k)
    {
      // if lower half
      if (sj <= si)
      {
        return moveLeft(sj);
      }
      // else upper half
      else
      {
        return moveDown(sj - si);
      }
    }
    // else not inside square, but to the right or below
    else
    {
      // if below
      if (si >= k)
      {
        int ri = si - k;
        if (k - sj >= ri + 2)
        {
          return moveUp(ri + 1);
        }
        else
        {
          if (sj == 0)
          {
            return moveUp(k - 1);
          }
          else
          {
            return moveLeft(sj);
          }
        }
      }
      // else to the right
      else
      {
        int i = ti - 1;
        int j = tj - 1;
        int l = 1;
        assert(i >= 0 && j >= 0);
        while (i >= 1 && i + j - 2 > dsum)
        {
          i--;
          j--;
          l++;
        }
        assert(j >= 0);
        return moveLeft(l);
      }
    }
  }
  // else to the left or above square
  else
  {
    // if to the left
    if (tj < dj)
    {
      int maxr = n - ti - 1;
      int maxup = 0;
      int i = ti;
      while (i - 1 + tj >= dsum && tj + maxup + 1 < n)
      {
        i--;
        maxup++;
      }
      if (tj + maxr >= dj || maxup == 0)
      {
        return moveRight(min_(maxr, dj - tj));
      }
      else
      {
        return moveUp(maxup);
      }
    }
    // else above square
    else
    {
      int l = 0;
      int j = tj;
      while (ti + j - 1 > dsum && ti + l + 1 < n)
      {
        j--;
        l++;
      }
      if (l > 0)
      {
        return moveDown(l);
      }
      else
      {
        return moveRight(1);
      }
    }
  }
}

int move()
{
  for (int p = 0; p < nn; ++p)
  {
    if (a[p] == t)
    {
      ti = p / n;
      tj = p % n;
      assert(ti + tj > di + dj || (ti + tj == di + dj && ti >= di));
      return move1();
    }
  }
  abort();
}

void solve()
{
  copy(a, a0);
  movec = 0;
  static int ts[900];
  for (int i = 0; i < 900; ++i)
  {
    ts[i] = i + 1;
  }
  int p = 0;
  int len = 1;
  int yet;
  while (len < n - 1)
  {
    yet = len;
    dsum = len - 1;
    for (di = 0; di < len; ++di)
    {
      dj = dsum - di;
      int q = p + (rand() % yet);
      if (q > p)
      {
        swap_(ts[p], ts[q]);
      }
      t = ts[p];
      if (!move())
      {
        return;
      }
      assert(a[di * n + dj] == t);
      p++;
      yet--;
    }
    len++;
  }
}

int main()
{
  stop_time = (clock_t)((double)clock() + (double)CLOCKS_PER_SEC * 1.8);
  srand(1);
  scanf("%d", &n);
  nn = n * n;
  for (int i = 0; i < nn; ++i)
  {
    scanf("%d", &a0[i]);
  }
  while (timeLeft())
  {
    solve();
    int score = get_score();
    if (score > best_score)
    {
      best_score = score;
      best_movec = movec;
      for (int i = 0; i < movec * 3; ++i)
      {
        best_moves[i] = moves[i];
      }
    }
  }
  //copy(a, a0);
  //for (int i = 0; i < best_movec; ++i)
  //{
  //  rot(best_moves[i * 3], best_moves[i * 3 + 1], best_moves[i * 3 + 2]);
  //}
  //print();
  printf("%d\n", best_movec);
  for (int i = 0; i < best_movec; ++i)
  {
    printf("%d %d %d\n", best_moves[i * 3] + 1, best_moves[i * 3 + 1] + 1, best_moves[i * 3 + 2]);
  }
  return 0;
}