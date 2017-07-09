// 2016-12-18
// roads-and-libraries, world codesprint 8
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define swap_(x, y) { int z = x; x = y; y = z; }

typedef long long ll;

typedef struct L
{
  int *xs;
  int n;
  int size;
} L;

void add(L *l, int x)
{
  if (l->n == l->size)
  {
    l->size *= 2;
    l->xs = realloc(l->xs, sizeof(int) * l->size);
    assert(l->xs);
  }
  l->xs[l->n++] = x;
}

void ini(L *l)
{
  l->n = 0;
  l->size = 4;
  l->xs = malloc(sizeof(int) * l->size);
  assert(l->xs);
}

L *create()
{
  L *l = malloc(sizeof(L));
  assert(l);
  ini(l);
  return l;
}
  
L *ls[100000];

ll solve()
{
  int n, m;
  ll rc, lc;
  scanf("%d%d%lld%lld", &n, &m, &lc, &rc);
  for (int i = 0; i < n; ++i)
  {
    ls[i] = NULL;
  }
  int count = 0;
  for (int _i = 0; _i < m; ++_i)
  {
    int i, j;
    scanf("%d%d", &i, &j);
    i--; j--;
    if (lc <= rc)
    {
      continue;
    }
    if (ls[i] == ls[j])
    {
      if (ls[i] == NULL)
      {
        L *l = create();
        add(l, i);
        add(l, j);
        count++;
        ls[i] = l;
        ls[j] = l;
      }
    }
    else
    {
      count++;
      if (ls[i] == NULL)
      {
        swap_(i, j);
      }
      if (ls[j] == NULL)
      {
        add(ls[i], j);
        ls[j] = ls[i];
      }
      else
      {
        if (ls[i]->n < ls[j]->n)
        {
          swap_(i, j);
        }
        L *l = ls[j];
        for (int p = 0; p < l->n; ++p)
        {
          int k = l->xs[p];
          add(ls[i], k);
          ls[k] = ls[i];
        }
        free(l->xs);
        free(l);
      }
    }
  }
  return rc * count + lc * (n - count);
}

int main()
{
  int q;
  scanf("%d", &q);
  for (int i = 0; i < q; ++i)
  {
    ll min_cost = solve();
    printf("%lld\n", min_cost);
  }
  return 0;
}