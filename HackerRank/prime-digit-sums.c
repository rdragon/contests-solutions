// 2016-12-19
// prime-digit-sums, world codesprint 8
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

typedef unsigned long long ull;
typedef unsigned int ui;

#define swap_(x, y) { ui *z = x; x = y; y = z; }

#define ac 20
#define ec (18 * 18)
#define mod 1000000007

#define brutec 13
ui brutes[] = {0,9,90,303,280,218,95,101,295,513,737,668,578}; // see haskell file

ui as[ec * ac];
ui startv[] = {17,6,6,2,0,0,9,3,9,3,0,0,6,4,13,15,13,15};
ui endv[] = {5,11,11,26,20,20,7,7,7,5,24,24,5,5,2,2,2,1};

// 18x18 times 18x18
void multmm(ui *a, ui *b, ui *c)
{
  for (int i = 0; i < 18; ++i)
  {
    for (int j = 0; j < 18; ++j)
    {
      ull x = 0;
      for (int k = 0; k < 18; ++k)
      {
        x += (ull)a[i * 18 + k] * (ull)b[k * 18 + j];
      }
      c[i * 18 + j] = (ui)(x % mod);
    }
  }
}

// 18x18 times 18x1
void multmv(ui *a, ui *b, ui *c)
{
  for (int i = 0; i < 18; ++i)
  {
    ull x = 0;
    for (int k = 0; k < 18; ++k)
    {
      x += (ull)a[i * 18 + k] * (ull)b[k];
    }
    c[i] = (ui)(x % mod);
  }
}

// 1x18 times 18x1
ui inprod(ui *a, ui *b)
{
  ull x = 0;
  for (int k = 0; k < 18; ++k)
  {
    x += (ull)a[k] * (ull)b[k];
  }
  return (ui)(x % mod);
}

void copy(ui *a, ui *b, int n)
{
  for (int i = 0; i < n; ++i)
  {
    a[i] = b[i];
  }
}

void ini()
{
  ui _a[] = {0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  ui _b[ec];
  ui *a = _a, *b = _b;
  for (int i = 0; i < ac; ++i)
  {
    copy(&as[i * ec], a, ec);
    multmm(a, a, b);
    swap_(a, b);
  }
}

ui solve(int n)
{
  if (n < brutec)
  {
    return brutes[n];
  }
  n -= 12;
  ui _v[18], _u[18];
  ui *v = _v, *u = _u;
  copy(v, startv, 18);
  ui *a = as;
  while (n)
  {
    if (n % 2)
    {
      multmv(a, v, u);
      swap_(v, u);
    }
    n /= 2;
    a += ec;
  }
  return inprod(v, endv);
}

int main()
{
  ini();
  int q;
  scanf("%d", &q);
  for (int i = 0; i < q; ++i)
  {
    int n;
    scanf("%d", &n);
    ui x = solve(n);
    printf("%u\n", x);
  }
  return 0;
}