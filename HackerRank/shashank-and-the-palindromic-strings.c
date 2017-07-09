// 2016-10-15
// shashank and the palindromic strings, week of code 24
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define md 1000000007

char cv[1001], fv[1000], lv[1000], gv[1000];
unsigned int mv[1000 * 1000 * 4];
int m;

void readInput() {
    int i, j, k, l, n;
    char *s;
    for (i = 0; i < 1000; ++i)
    {
        fv[i] = 0;
        lv[i] = 0;
    }
    s = cv;
    k = 0;
    scanf("%d", &n);
    for (i = 0; i < n; ++i)
    {
        scanf("%s", s);
        l = strlen(s);
        fv[k] = 1;
        lv[k + l - 1] = 1;
        for (j = 0; j < l; ++j)
        {
            gv[k + j] = i;
        }
        k += l;
        s += l;
    }
    m = strlen(cv);
}

int ix(int i, int j, int oi, int oj) {
    return (i * m * 4) + (j * 4) + (oi * 2) + oj;
}

unsigned int f(int i, int j, int oi, int oj) {
    if (i == j)
    {
        return (oi || oj) ? 2 : 1;
    }
    if (j < i)
    {
        return 1;
    }
    if (gv[i] == gv[j])
    {
        oi = oi || oj;
        oj = oi;
    }
    return mv[ix(i, j, oi, oj)];
}

void run() {
    int l, i, j, p;
    int il, jf, oi, oj, oi1, oj1, b1, b2;
    unsigned int c;
    readInput();
    for (l = 2; l <= m; ++l)
    {
        for (i = 0; i <= m - l; ++i)
        {
            j = i + l - 1;
            for (p = 0; p < 4; ++p)
            {
                if (p == 0 || p == 3 || gv[i] < gv[j])
                {
                    il = lv[i];
                    jf = fv[j];
                    oi = p & 1;
                    oj = p >> 1;
                    c = 0;
                    b1 = oi || !il;
                    b2 = oj || !jf;
                    oi1 = !il && oi;
                    oj1 = !jf && oj;
                    if (b1)
                    {
                        c += f(i + 1, j, oi1, oj);
                    }
                    if (b2)
                    {
                        c += f(i, j - 1, oi, oj1);
                    }
                    if (b1 && b2 && (l > 2 || oi || oj))
                    {
                        c += md - f(i + 1, j - 1, oi1, oj1);
                    }
                    if (cv[i] == cv[j])
                    {
                        c += f(i + 1, j - 1, !il, !jf);
                    }
                    mv[ix(i, j, oi, oj)] = c % md;
                }
            }
        }
    }
    printf("%u\n", mv[ix(0, m - 1, 0, 0)]);
}

int main() {
    int q, i;
    scanf("%d", &q);
    for(i = 0; i < q; ++i) {
        run();
    }
    return 0;
}