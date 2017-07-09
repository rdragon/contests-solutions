// 2016-11-26

#ifdef LCL
#include <simple2d.h>
#else
#define NDEBUG
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include <string.h>

#define min_(x, y) ((y) < (x) ? (y) : (x))
#define max_(x, y) ((y) > (x) ? (y) : (x))
#define between_(x, y, z) ((x) <= (y) && (y) <= (z))
#define swapE_(x, y) { E z = x; x = y; y = z; }

// game parameters
#define width 16000.0
#define height 7500.0
#define goalY (height / 2)
#define homeY goalY
#define goalR 2000.0
#define maxThrust 150.0
#define maxPower 500.0
#define bludgerPower 1000.0
#define accioPower 3000.0
#define flipendoPower 6000.0

// ai
#define simC 7
#define eqTreshold 5.0
#define detourFactor 1.1
#define enable_spells 1
#define stupidAi 0

// drawing
#define zoom 10.0 // zoom factor
#define startLive 0
#define startLoop 0
#define startSpeed 5 // initial ticks per second during play
#define speedUp 2.0
#define startFrame 0
#define showDiff 1
#define showCollisions 0
#define initialShowB 1

// other
#define pi 3.14159265359
#define inf INFINITY
#define eps (1e-7)
#define horCol (-1)
#define verCol (-2)
#define bIx 4 // index of first bludger in entity array
#define sIx 6 // index of first snaffle in entity array
#define maxEc 13 // max entity count
#define maxGmC 210
#define submit 0
#define infMana 0
#define createAsym 0.9
#define lclBattle 0

typedef struct E // entity
{
  int id;
  char c; // one of WOBSP (wizard, opponent, bludger, snaffle, pole)
  double x;
  double y;
  double vx;
  double vy;

  char action; // one of MTOPAF (move, throw, obliviate..) or '\0'. needs manual reset. also used for bludgers
  double tx; // target x
  double ty;
  int power; // power or target id

  int snaffleId; // for wizards: id of grabbed snaffle during this tick. is reset during start of next tick
  int wizId; // for snaffles: id of wizard that grabbed the snaffle during this tick. is reset at end of tick
              // for bludgers: id of last wizard that collided with this bludger
  int catchTick; // for wizards: tick of last catch
  int catchTickBackup;
} *E;

typedef struct Sp // spell
{
  int id;
  int tid; // target id
  int tick;
} Sp;

typedef struct GmStruct // game
{
  int ec; // entity count
  struct E es[maxEc]; // entities
  int tick;
  int scores[2];
  int manas[2];
  int petrifieds[2];
  Sp spells[6]; // oblivate, accio, flipendo
  double time; // only for drawing
} GmStruct;
typedef GmStruct *Gm;

typedef struct Box
{
  double x1;
  double y1;
  double x2;
  double y2;
} Box;

typedef struct Collision
{
  int i;
  int j;
} Collision;

clock_t start;
GmStruct gms[1];
Gm liveGm = &gms[0];
struct E poles[4];
int away;
double pointX;
double pointY;
int exceptId;
char xChar;
int xId;
double homeX;
double goalX;
int wIx;
int oIx;
int simThrow = 1;
int startUp = 1;

#ifdef LCL
int lcl = 1;
S2D_Window *window;
GmStruct gmsR[maxGmC]; // read games 
GmStruct gmsS[maxGmC]; // simulated games 
GmStruct gmsX[maxGmC]; // extra games 
Gm gmsB; // games background
Gm gmsF; // games foreground
int gmC;
int loopTick = -1;
int hideB = !initialShowB;
int loopWidth = 1;
int live = startLive;
int showExtra;
int gmTick;
double speed = startSpeed;
#else
int lcl = 0;
void runLcl() { }
#endif

void assertE(E e)
{
  assert(isfinite(e->x));
  assert(isfinite(e->y));
  assert(isfinite(e->vx));
  assert(isfinite(e->vy));
}

void assertGm(Gm gm)
{
  assert(gm->ec >= sIx);
  assert(gm->ec <= maxEc);
  for (int i = 0; i < gm->ec; ++i)
  {
    assertE(&gm->es[i]);
  }
}

void clearActions(Gm gm)
{
  for (int i = 0; i < bIx; ++i)
  {
    E w = &gm->es[i];
    w->action = '\0';
  }
}

double getElapsed()
{
  clock_t end = clock();
  return ((double)(end - start)) / CLOCKS_PER_SEC;
}

int rnd(double x)
{
  return (int)round(x);
}

double norm(double x, double y)
{
  return sqrt(x * x + y * y);
}

double normSq(double x, double y)
{
  return x * x + y * y;
}

double distSq(E e, E f)
{
  return normSq(e->x - f->x, e->y - f->y);
}

double dist(E e, E f)
{
  return norm(e->x - f->x, e->y - f->y);
}

int isWiz(E e)
{
  return e->c == 'W' || e->c == 'O';
}

int isW(E e)
{
  return e->c == (away ? 'O' : 'W');
}

int isO(E e)
{
  return e->c == (away ? 'W' : 'O');
}

int isS(E e)
{
  return e->c == 'S';
}

int isXExcept(E e)
{
  return (e->c == xChar) && (e->id != exceptId);
}

int hasIdWiz(E e)
{
  return e->wizId == xId;
}

int isB(E e)
{
  return e->c == 'B';
}

int eq(E e, E f)
{
  double a = eqTreshold * eqTreshold;
  return distSq(e, f) < a
    && normSq(e->vx - f->vx, e->vy - f->vy) < a;
}

int hasSnaffle(E e)
{
  return e->snaffleId >= 0;
}

void forbidAllyActions(Gm gm)
{
  assert(gm->es[wIx].action == '\0');
  assert(gm->es[wIx + 1].action == '\0');
}

int getTeam(E w)
{
  assert(isWiz(w));
  return w->c == 'O';
}

double getRadius(char c)
{
  switch (c)
  {
    case 'W': 
    case 'O': return 400;
    case 'S': return 150;
    case 'B': return 200;
    case 'P': return 300;
    default: abort();
  }
}

double getMass(char c)
{
  switch (c)
  {
    case 'W': 
    case 'O': return 1;
    case 'S': return 0.5;
    case 'B': return 8;
    case 'P': return inf;
    default: abort();
  }
}

double getFriction(char c)
{
  switch (c)
  {
    case 'W': 
    case 'O': return 0.75;
    case 'S': return 0.75;
    case 'B': return 0.9;
    case 'P': return inf;
    default: abort();
  }
}

int getMana(char c)
{
  switch (c)
  {
    case 'O': return 5;
    case 'P': return 10;
    case 'A': return 15;
    case 'F': return 20;
    default: abort();
  }
}

int getDur(char c)
{
  switch (c)
  {
    case 'O': return 4;
    case 'A': return 6;
    case 'F': return 3;
    default: abort();
  }
}

int isSWiz(char c1, char c2)
{
  return ((c1 == 'S' && (c2 == 'W' || c2 == 'O'))
    || (c2 == 'S' && (c1 == 'W' || c1 == 'O')));
}

int isBWiz(char c1, char c2)
{
  return ((c1 == 'B' && (c2 == 'W' || c2 == 'O'))
    || (c2 == 'B' && (c1 == 'W' || c1 == 'O')));
}

E getE(Gm gm, int id)
{
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    if (e->id == id)
    {
      return e;
    }
  }
  return NULL;
}

// game snaffle count
int gmSc(Gm gm)
{
  return gm->ec - sIx;
}

// game original snaffle count
int gmInSc(Gm gm)
{
  return gm->scores[0] + gm->scores[1] + gmSc(gm);
}

int hasWin(Gm gm, int team)
{
  return gm->scores[team] > gmInSc(gm) / 2;
}

int comp(Gm gm1, Gm gm2)
{
  int win1 = hasWin(gm1, away);
  int win2 = hasWin(gm2, away);
  if (win1 != win2)
  {
    return win1 ? 1 : -1;
  }
  else if (win1)
  {
    return 0;
  }
  int lose1 = hasWin(gm1, 1 - away);
  int lose2 = hasWin(gm2, 1 - away);
  if (lose1 != lose2)
  {
    return lose2 ? 1 : -1;
  }
  else if (lose1)
  {
    return 0;
  }
  int d = gm1->scores[away] - gm2->scores[away] + gm2->scores[1 - away] - gm1->scores[1 - away];
  if (d)
  {
    return d;
  }
  if (gmSc(gm1) == 1 && gmSc(gm2) == 1)
  {
    return (int)(gm1->es[sIx].x - gm2->es[sIx].x) * (away ? -1 : 1);
  }
  else
  {
    return 0;
  }
}

int comp1(Gm gm1, Gm gm2, int id)
{
  int x = comp(gm1, gm2);
  if (x)
  {
    return x;
  }
  E e1 = getE(gm1, id);
  E e2 = getE(gm2, id);
  if (e1 && e2)
  {
    return (int)(e1->x - e2->x) * (away ? -1 : 1);
  }
  return 0;
}

int snaffleCompar(const void *s1_in, const void *s2_in)
{
  E s1 = (E)s1_in;
  E s2 = (E)s2_in;
  return ((int)s1->x - (int)s2->x) * (away ? -1 : 1);
}

Sp *getSp(Gm gm, char c, int team)
{
  switch (c)
  {
  case 'O': return &gm->spells[team];
  case 'A': return &gm->spells[team + 2];
  case 'F': return &gm->spells[team + 4];
  }
  return NULL;
}

int getBludgerId(Gm gm, int largerOne)
{
  int id = -1;
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    if (e->c == 'B')
    {
      if (id == -1)
      {
        id = e->id;
      }
      else
      {
        return (id < e->id) != largerOne ? id : e->id;
      }
    }
  }
  return id;
}

E getMax(Gm gm, int(*filter)(E), double(*func)(E))
{
  double best = -inf;
  int j = -1;
  for (int i = 0; i < gm->ec; ++i)
  {
    if (!filter(&gm->es[i]))
      continue;
    double score = func(&gm->es[i]);
    if (score > best)
    {
      best = score;
      j = i;
    }
  }
  return j == -1 ? NULL : &gm->es[j];
}

E getMin(Gm gm, int(*filter)(E), double(*func)(E))
{
  double best = inf;
  int j = -1;
  for (int i = 0; i < gm->ec; ++i)
  {
    if (!filter(&gm->es[i]))
      continue;
    double score = func(&gm->es[i]);
    if (score < best)
    {
      best = score;
      j = i;
    }
  }
  return j == -1 ? NULL : &gm->es[j];
}

double getVx(E e)
{
  return e->vx;
}

double getId(E e)
{
  return e->id;
}

double pointDistSq(E e)
{
  return normSq(e->x - pointX, e->y - pointY);
}

void setPoint(double x, double y)
{
  pointX = x;
  pointY = y;
}

int isPetrified(Gm gm, int id)
{
  return id == gm->petrifieds[0] || id == gm->petrifieds[1];
}

void clearSpells(Gm gm)
{
  gm->petrifieds[0] = -1;
  gm->petrifieds[1] = -1;
  for (int i = 0; i < 6; ++i)
  {
    gm->spells[i].tick = -1;
  }
}

void posVelCopy(Gm gm, Gm gm1)
{
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    E f = getE(gm1, e->id);
    if (f)
    {
      e->x = f->x;
      e->y = f->y;
      e->vx = f->vx;
      e->vy = f->vy;
    }
  }
}

void copyActions(Gm gm, Gm gm1)
{
	for (int i = 0; i < 4; ++i)
	{
		E w = &gm->es[i];
		E w1 = &gm1->es[i];
		w->action = w1->action;
		w->tx = w1->tx;
		w->ty = w1->ty;
		w->power = w1->power;
	}
}

void stopAccioIfNeeded(Gm gm, E w)
{
  assert(isWiz(w));
  assert(w->snaffleId >= 0);
  int team = getTeam(w);
  Sp *sp = getSp(gm, 'A', team);
  assert(sp);
  if (sp->id == w->id && sp->tid == w->snaffleId)
  {
    sp->tick = -1;
  }
}

void fixGm(Gm gm, Gm gm1)
{
	gm->tick = gm1->tick;
  gm->scores[0] = gm1->scores[0];
  gm->scores[1] = gm1->scores[1];
  gm->manas[0] = gm1->manas[0];
  gm->manas[1] = gm1->manas[1];
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    e->snaffleId = -1;
    E f = getE(gm1, e->id);
    if (f)
    {
      e->x = f->x;
      e->y = f->y;
      e->vx = f->vx;
      e->vy = f->vy;
      if (isB(e))
      {
        e->wizId = f->wizId;
      }
    }
    else
    {
      assert(isS(e));
      if (i < gm->ec - 1)
      {
        gm->es[i] = gm->es[gm->ec - 1];
        i--;
      }
      gm->ec--;
    }
  }
  for (int i = 0; i < bIx; ++i)
  {
    E w1 = &gm1->es[i];
    if (w1->snaffleId >= 0)
    {
      E w = getE(gm, w1->id);
      assert(w && isWiz(w));
      w->snaffleId = w1->snaffleId;
      w->catchTick = gm->tick - 1;
      stopAccioIfNeeded(gm, w);
    }
  }
}

void printE(E e)
{
  printf("%d %c (%d, %d) (%d, %d) %d (%c %d %d %d) (%d %d)\n", e->id, e->c,
    rnd(e->x), rnd(e->y),
    rnd(e->vx), rnd(e->vy),
    e->snaffleId, e->action, rnd(e->tx), rnd(e->ty), e->power, e->wizId, e->catchTick);
}

void printGm(Gm gm)
{
  printf("ec = %d, tick = %d, time = %lf, scores = %d %d, manas = %d %d\n", gm->ec, gm->tick, gm->time, gm->scores[0], gm->scores[1], gm->manas[0], gm->manas[1]);
  for (int i = 0; i < gm->ec; ++i)
  {
    printE(&gm->es[i]);
  }
}

void printDiff(Gm gm, Gm gm1)
{
  if (gm->ec != gm1->ec)
  {
    printf("different length\n");
    return;
  }
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    int hit = 0;
    for (int j = 0; j < gm->ec; ++j)
    {
      E f = &gm1->es[j];
      if (e->id == f->id)
      {
        hit = 1;
        if (e->x != f->x || e->y != f->y || e->vx != f->vx || e->vy != f->vy)
        {
          printE(e);
          printE(f);
        }
        break;
      }
    }
    if (!hit)
    {
      printf("no match found for:\n");
      printE(e);
    }
  }
}

void printAction(E w)
{
  if (w->action == 'M')
  {
    printf("MOVE %d %d %d\n", (int)round(w->tx), (int)round(w->ty), w->power);
  }
  if (w->action == 'T')
  {
    printf("THROW %d %d %d\n", (int)round(w->tx), (int)round(w->ty), w->power);
  }
  if (w->action == 'O')
  {
    printf("OBLIVIATE %d\n", w->power);
  }
  if (w->action == 'P')
  {
    printf("PETRIFICUS %d\n", w->power);
  }
  if (w->action == 'A')
  {
    printf("ACCIO %d\n", w->power);
  }
  if (w->action == 'F')
  {
    printf("FLIPENDO %d\n", w->power);
  }
}

void printActions(Gm gm)
{
  for (int i = 0; i < gm->ec; ++i)
  {
    E w = &gm->es[i];
    if (isW(w))
    {
      printAction(w);
    }
  }
}

int diff(Gm gm, Gm gm1)
{
  if (gm->ec != gm1->ec) return 1;
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    int hit = 0;
    for (int j = 0; j < gm->ec; ++j)
    {
      E f = &gm1->es[j];
      if (e->id == f->id)
      {
        hit = 1;
        if (e->x != f->x || e->y != f->y || e->vx != f->vx || e->vy != f->vy)
        {
          return 1;
        }
        break;
      }
    }
    if (!hit)
    {
      return 1;
    }
  }
  return 0;
}

void iniE(E e)
{
  *e = (struct E) { 0 };
  e->catchTick = -1;
  e->wizId = -1;
  e->snaffleId = -1;
}

void orderGm(Gm gm)
{
  GmStruct _gm1 = *gm;
  Gm gm1 = &_gm1;
  int j = 0;
  const char *order = "WOBS";
  for (int k = 0; k < strlen(order); ++k)
  {
    char c = order[k];
    for (int i = 0; i < gm1->ec; ++i)
    {
      E e = &gm1->es[i];
      if (e->c == c)
      {
        gm->es[j++] = *e;
      }
    }
  }
  assert(j == gm->ec);
}

void readEs(Gm gm, int n)
{
  char buf[20];
  int grabCounter = 0;
  for (int i = 0; i < n; ++i)
  {
    E e = &gm->es[i];
    iniE(e);
    int state;
    scanf("%d%s%lf%lf%lf%lf%d", &e->id, buf, &e->x, &e->y, &e->vx, &e->vy, &state);
    if (!away && !lcl && !submit)
      fprintf(stderr, "%d %c %d %d %d %d %d\n", e->id, buf[0], (int)e->x, (int)e->y, (int)e->vx, (int)e->vy, state);
    e->c = buf[0];
    if (away && isWiz(e))
    {
      e->c = (e->c == 'W') ? 'O' : 'W';
    }
    if (isWiz(e) && state)
    {
      e->snaffleId = 0;
      grabCounter++;
    }
    if (isS(e) && state)
    {
      e->wizId = 0;
      grabCounter--;
    }
    if (isB(e))
    {
      e->wizId = state;
    }
  }
  assert(!grabCounter);
  gm->ec = n;
  orderGm(gm);
  for (int i = 0; i < bIx; ++i)
  {
    E w = &gm->es[i];
    if (w->snaffleId == 0)
    {
      setPoint(w->x, w->y);
      xId = 0;
      E s = getMin(gm, hasIdWiz, pointDistSq);
      assert(s && isS(s));
      assert(s->x == w->x && s->y == w->y);
      w->snaffleId = s->id;
      s->wizId = -1;
    }
  }
}

void iniGm(Gm gm)
{
  *gm = (GmStruct) { 0 };
  clearSpells(gm);
  if (infMana)
  {
    gm->manas[0] = 99999999;
    gm->manas[1] = 99999999;
  }
}

void readGm(Gm gm)
{
  iniGm(gm);
  static int tick;
  gm->tick = tick++;
  scanf("%d%d%d%d", &gm->scores[away], &gm->manas[away]
                  , &gm->scores[1 - away], &gm->manas[1 - away]);
  int n;
  scanf("%d", &n);
  if (!away && !lcl)
  {
    fprintf(stderr, "%d %d\n%d %d\n%d\n", gm->scores[0], gm->manas[0], gm->scores[1], gm->manas[1], n);
  }
  assert(n <= maxEc);
  readEs(gm, n);
  if (lcl)
  {
    char buf[50];
    for (int team = 0; team < 2; ++team)
    {
      for (int i = 0; i < 2; ++i)
      {
        E w = &gm->es[(team ? oIx : wIx) + i];
        scanf("%s%lf%lf%d", buf, &w->tx, &w->ty, &w->power);
        w->action = buf[0];
      }
      scanf("%d%d%s%s%s", &n, &n, buf, buf, buf);
    }
  }
}

void mkPole(E e, double x, double y)
{
  iniE(e);
  e->x = x;
  e->y = y;
  e->c = 'P';
}

double getCircleCircleCollision(double x1, double y1, double vx, double vy, double r)
{
  if (vx == 0.0 && vy == 0.0)
    return (x1 * x1 + y1 * y1 < r * r - eps) ? 0 : inf;
  if (vx * x1 + vy * y1 > -eps) return inf;
  double a = vy;
  double b = -vx;
  double c = vx * y1 - vy * x1;
  double e = 1.0 / (a * a + b * b);
  double p = -c * a * e;
  double q = -c * b * e;
  double d = p * p + q * q;
  double s = r * r;
  if (d > s - eps)
    return inf;
  double t = (p - x1) * (p - x1) + (q - y1) * (q - y1);
  double u = sqrt(t * e) - sqrt(max_(0, s - d) * e);
  return max_(0, u);
}

void resolveCircleCircleElastic(double x1, double y1, double *vx1, double *vy1, double m1
                              , double x2, double y2, double *vx2, double *vy2, double m2)
{
  double dx = x2 - x1;
  double dy = y2 - y1;
  double d = sqrt(dx * dx + dy * dy);
  if (d < eps) return;
  double nx = dx / d;
  double ny = dy / d;
  double p = 2 * ((*vx1 - *vx2) * nx + (*vy1 - *vy2) * ny) / (m1 + m2) * m1 * m2;
  if (p < 200)
  {
  	p = (p + 200) / 2;
  }
  *vx1 -= p / m1 * nx;
  *vy1 -= p / m1 * ny;
  *vx2 += p / m2 * nx;
  *vy2 += p / m2 * ny;
}

void resolveCircleStaticCircleElastic(double x1, double y1, double *vx1, double *vy1
                                    , double x2, double y2)
{
  double dx = x2 - x1;
  double dy = y2 - y1;
  double d = sqrt(dx * dx + dy * dy);
  if (d < eps) return;
  double nx = dx / d;
  double ny = dy / d;
  double p = 2 * (*vx1 * nx + *vy1 * ny);
  if (p < 400)
  {
  	p = (p + 400) / 2;
  }
  *vx1 -= p * nx;
  *vy1 -= p * ny;
}

void applyThrust(E e)
{
  if (e->action == 'M')
  {
    double dx = e->tx - e->x;
    double dy = e->ty - e->y;
    if (dx != 0 || dy != 0)
    {
      double a = (double)e->power / getMass(e->c) / norm(dx, dy);
      e->vx += dx * a;
      e->vy += dy * a;
    }
  }
}

void applyFriction(E e)
{
  e->vx *= getFriction(e->c);
  e->vy *= getFriction(e->c);
}

int canCatch(Gm gm, E e)
{
  return !(e->catchTick >= 0 && gm->tick - e->catchTick < 3);
}

double getCol(Gm gm, int i, int j)
{
  assert(i >= -4 && i < gm->ec && j >= 0 && j < gm->ec);
  E e = i < 0 ? &poles[4 + i] : &gm->es[i];
  E f = &gm->es[j];
  double r = getRadius(e->c) + getRadius(f->c);
  if (isSWiz(e->c, f->c))
  {
    if (!canCatch(gm, e) || !canCatch(gm, f))
    {
      return inf;
    }
    else
    {
      r -= getRadius('S') + 1;
    }
  }
  return getCircleCircleCollision(e->x - f->x, e->y - f->y, e->vx - f->vx, e->vy - f->vy, r);
}

void handleBounce(Gm gm, int i, int j)
{
  assert(i >= -4 && i < gm->ec && j >= 0 && j < gm->ec);
  E e = i < 0 ? &poles[4 + i] : &gm->es[i];
  E f = &gm->es[j];
  if (isSWiz(e->c, f->c))
  {
    if (e->c == 'S')
    {
      handleBounce(gm, j, i);
    }
    else
    {
      f->wizId = e->id;
      e->catchTick = gm->tick;
    }
  }
  else
  {
  	if (showCollisions && !startUp)
  	{
  		printf("%c (%d) - %c (%d)\n", e->c, e->id, f->c, f->id);
  	}
    if (isBWiz(e->c, f->c))
    {
      if (e->c == 'B')
      {
        e->wizId = f->id;
      }
      else
      {
        f->wizId = e->id;
      }
    }
    if (e->c == 'P')
    {
      resolveCircleStaticCircleElastic(f->x, f->y, &f->vx, &f->vy, e->x, e->y);
    }
    else if (f->c == 'P')
    {
      resolveCircleStaticCircleElastic(e->x, e->y, &e->vx, &e->vy, f->x, f->y);
    }
    else
    {
      resolveCircleCircleElastic(e->x, e->y, &e->vx, &e->vy, getMass(e->c)
        , f->x, f->y, &f->vx, &f->vy, getMass(f->c));
    }
  }
}

void simGm(Gm gm, double t)
{
  assertGm(gm);
  int counter = 0;
  while (++counter)
  {
    assert(counter < 999);
    if (counter >= 999)
      break;
    double t_before = t;
    Box _bs[maxEc + 4];
    Box *bs = &_bs[4];
    Collision col = { 0, 0 };
    for (int i = 0; i < gm->ec; ++i)
    {
      E e = &gm->es[i];
      double r = getRadius(e->c);
      if (e->vx != 0.0 && (e->c != 'S' || fabs(e->y - goalY) > goalR))
      {
        double s = e->vx > 0 ? (width - r - e->x) / e->vx : (r - e->x) / e->vx;
        if (s < t)
        {
          t = max_(0, s);
          col = (Collision) { i, horCol };
        }
      }
      if (e->vy != 0.0)
      {
        double s = e->vy > 0 ? (height - r - e->y) / e->vy : (r - e->y) / e->vy;
        if (s < t)
        {
          t = max_(0, s);
          col = (Collision) { i, verCol };
        }
      }
    }
    for (int i = -4; i < gm->ec; ++i)
    {
      E e = i < 0 ? &poles[4 + i] : &gm->es[i];
      if (isS(e) && e->wizId >= 0)
      {
        assert(isS(e));
        bs[i] = (Box) { 0, 0, 0, 0 };
      }
      else
      {
        double r = getRadius(e->c);
        double x = e->x + e->vx * t;
        double y = e->y + e->vy * t;
        bs[i] = (Box) {
          min_(e->x, x) - r
            , min_(e->y, y) - r
            , max_(e->x, x) + r
            , max_(e->y, y) + r
        };
      }
    }
    for (int i = -4; i < gm->ec; ++i)
    {
      Box b1 = bs[i];
      for (int j = max_(0, i + 1); j < gm->ec; ++j)
      {
        Box b2 = bs[j];
        if (b1.x1 < b2.x2 && b1.x2 > b2.x1 &&
          b1.y1 < b2.y2 && b1.y2 > b2.y1)
        {
          double s = getCol(gm, i, j);
          if (s < t)
          {
            t = s;
            col = (Collision) { i, j };
          }
        }
      }
    }
    for (int i = 0; i < gm->ec; ++i)
    {
      gm->es[i].x += gm->es[i].vx * t;
      gm->es[i].y += gm->es[i].vy * t;
    }
    if (t < t_before)
    {
      switch (col.j)
      {
      case horCol:
        assert(between_(0, col.i, gm->ec));
        gm->es[col.i].vx *= -1;
        break;
      case verCol:
        assert(between_(0, col.i, gm->ec));
        gm->es[col.i].vy *= -1;
        break;
      default:
        handleBounce(gm, col.i, col.j);
      }
      t = t_before - t;
    }
    else break;
  }
  assertGm(gm);
}

void directBludger(Gm gm, E b)
{
  double best = inf;
  int j = -1;
  E w;
  for (int i = 0; i < bIx; ++i)
  {
    w = &gm->es[i];
    double sq = normSq(w->x - b->x, w->y - b->y);
    int team = getTeam(w);
    Sp *sp = getSp(gm, 'O', team);
    if (sq < best && b->wizId != w->id && (sp->tid != b->id || sp->tick < gm->tick))
    {
      best = sq;
      j = i;
    }
  }
  if (j == -1)
  {
    b->action = '\0';
  }
  else
  {
    w = &gm->es[j];
    b->action = 'M';
    b->tx = w->x;
    b->ty = w->y;
    b->power = (int)bludgerPower;
  }
}

void beginStep(Gm gm, int full_sim)
{
  // handle throw action, reset snaffleId, set catchTickBackup
  for (int i = 0; i < bIx; ++i)
  {
    E w = &gm->es[i];
    w->catchTickBackup = w->catchTick;
    if (w->action == 'T')
    {
      E s = getE(gm, w->snaffleId);
      assert(s && isS(s));
      double dx = w->tx - w->x;
      double dy = w->ty - w->y;
      double p = w->power / norm(dx, dy) / getMass('S');
      s->vx += dx * p;
      s->vy += dy * p;
    }
    w->snaffleId = -1;
  }

  directBludger(gm, &gm->es[bIx]);
  directBludger(gm, &gm->es[bIx + 1]);

  // apply thrust
  for (int i = 0; i < sIx; ++i)
  {
    E w_or_b = &gm->es[i];
    applyThrust(w_or_b);
  }

  // apply petrificus
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    if (isPetrified(gm, e->id))
    {
      e->vx = 0;
      e->vy = 0;
    }
  }

  // apply accio and flipendo
  for (int k = 2; k < 6; ++k)
  {
    Sp *sp = &gm->spells[k];
    if (gm->tick <= sp->tick)
    {
      E e = getE(gm, sp->id);
      E f = getE(gm, sp->tid);
      if (e != NULL && f != NULL)
      {
        double dx = e->x - f->x;
        double dy = e->y - f->y;
        double d = norm(dx, dy);
        if (d > 0)
        {
          double power = (k < 4 ? accioPower : flipendoPower) / d * 1000 / d * 1000;
          power = min_(1000, power);
          double a = power / d / getMass(f->c) * (k < 4 ? -1 : 1);
          f->vx -= dx * a;
          f->vy -= dy * a;
        }
      }
    }
  }

  // handle start-of-tick-grab (as the collision engine does not correctly work for overlapping entities)
  for (int i = 0; i < bIx; ++i)
  {
    E w = &gm->es[i];
    if (canCatch(gm, w))
    {
      setPoint(w->x, w->y);
      E s = getMin(gm, isS, pointDistSq);
      if (s != NULL)
      {
        double r = getRadius('W') - 1;
        if (normSq(s->x - w->x, s->y - w->y) <= r * r)
        {
          s->wizId = w->id;
          w->catchTick = gm->tick;
        }
      }
    }
  }
}

void updateSpells(Gm gm)
{
  gm->petrifieds[0] = -1;
  gm->petrifieds[1] = -1;
  for (int i = 0; i < bIx; ++i)
  {
    E w = &gm->es[i];
    int team = getTeam(w);
    if (w->action == 'P')
    {
      gm->petrifieds[team] = w->power;
    }
    else if (w->action == 'O')
    {
      Sp *sp = getSp(gm, 'O', team);
      sp->tid = w->power;
      sp->tick = gm->tick + getDur('O');
    }
    else if (w->action == 'A')
    {
      Sp *sp = getSp(gm, 'A', team);
      sp->id = w->id;
      sp->tid = w->power;
      sp->tick = gm->tick + getDur('A');
    }
    else if (w->action == 'F')
    {
      Sp *sp = getSp(gm, 'F', team);
      sp->id = w->id;
      sp->tid = w->power;
      sp->tick = gm->tick + getDur('F');
    }
  }
}

void endStep(Gm gm, int full_sim)
{
  for (int i = 0; i < gm->ec; ++i)
  {
    applyFriction(&gm->es[i]);
  }

  // set correct snaffleId and move catched snaffle
  for (int i = sIx; i < gm->ec; ++i)
  {
    E s = &gm->es[i];
    if (s->wizId >= 0)
    {
      E w = getE(gm, s->wizId);
      assert(isWiz(w));
      s->wizId = -1;
      s->x = w->x;
      s->y = w->y;
      s->vx = w->vx;
      s->vy = w->vy;
      if (full_sim)
      {
        w->snaffleId = s->id;
      }
    }
  }

  // update score and remove scored snaffles
  if (full_sim)
  {
    for (int i = sIx; i < gm->ec; ++i)
    {
      E s = &gm->es[i];
      if (s->x < -eps || s->x > width + eps)
      {
        gm->scores[s->x < -eps]++;
        //printf("new score: %d vs %d\n", gm->scores[0], gm->scores[1]);
        if (i < gm->ec - 1)
        {
          gm->es[i] = gm->es[gm->ec - 1];
          i--;
        }
        gm->ec--;
      }
    }
  }

  updateSpells(gm);

  if (!full_sim)
  {
    for (int i = 0; i < bIx; ++i)
    {
      E w = &gm->es[i];
      w->catchTick = w->catchTickBackup;
    }
  }
  else
  {
    for (int i = 0; i < bIx; ++i)
    {
      E w = &gm->es[i];
      if (w->catchTick == gm->tick)
      {
        stopAccioIfNeeded(gm, w);
      }
    }
  }
  
  clearActions(gm);

  // finish off
  for (int i = 0; i < gm->ec; ++i)
  {
    E e = &gm->es[i];
    e->x = round(e->x);
    e->y = round(e->y);
    e->vx = round(e->vx);
    e->vy = round(e->vy);
  }
  gm->tick++;
  if (full_sim)
  {
    gm->manas[0]++;
    gm->manas[1]++;
  }
}

void step(Gm gm, int full_sim)
{
  beginStep(gm, full_sim);
  simGm(gm, 1);
  endStep(gm, full_sim);
}

void getPoleThrowLoc(double *tx, double *ty, double x, double y)
{
  double left = x < width / 2;
  double down = y < goalY;
  double px = left ? 0 : width;
  double py = goalY + goalR * (down ? -1 : 1);
  double dx = px - x;
  double dy = py - y;
  double c2 = normSq(dx, dy);
  double c = sqrt(c2);
  assert(c);
  double a = getRadius('P') + getRadius('S') + 10;
  double b = sqrt(c2 - a * a);
  double t = b / c;
  dx *= t;
  dy *= t;
  assert(a / c <= 1);
  assert(a / c >= -1);
  double alpha = asin(a / c) * (left == down ? -1 : 1);
  *tx = x + cos(alpha) * dx - sin(alpha) * dy;
  *ty = y + sin(alpha) * dx + cos(alpha) * dy;
}

void getCorrectThrowLoc(double *x_out, double *y_out, double x, double y, double vx, double vy, double tx, double ty, double power)
{
  double p = (power - 1) * (power - 1);
  double v_norm_sq = normSq(vx, vy);
  if (v_norm_sq > p)
  {
    double q = p / sqrt(v_norm_sq);
    vx *= q;
    vy *= q;
    v_norm_sq = normSq(vx, vy);
  }
  double ax = tx - x;
  double ay = ty - y;
  double a = normSq(ax, ay);
  double b = -2 * (ax * vx + ay * vy);
  double c = v_norm_sq - power * power;
  double d = b * b - 4 * a * c;
  if (d < 0)
  {
    *x_out = tx;
    *y_out = ty;
    return;
  }
  double alpha = (sqrt(d) - b) / 2 / a;
  *x_out = alpha * ax - vx + x;
  *y_out = alpha * ay - vy + y;
}

void findThrowLoc(Gm gm_in, int id)
{
  static GmStruct gms[4];
  int sim_c = 4;
  Gm const gm = &gms[0];
  Gm const gm_best = &gms[1];
  Gm const gm_ans = &gms[2];
  Gm const gm_b = &gms[3];
  *gm = *gm_in;
  E w = getE(gm, id);
  assert(w && isWiz(w));
  int s_id = w->snaffleId;
  assert(s_id >= 0);
  w->action = 'T';
  w->power = (int)maxPower;
  *gm_in = *gm;
  if (norm(w->x - goalX, w->y - goalY) > width / 2)
  {
    w->tx = goalX;
    w->ty = w->y;
  }
  else
  {
    double tx, ty;
    if (fabs(w->y - goalY) < goalR - getRadius('S') - getRadius('P'))
    {
      tx = goalX;
      ty = w->y;
    }
    else
    {
      getPoleThrowLoc(&tx, &ty, w->x, w->y);
    }
    getCorrectThrowLoc(&w->tx, &w->ty, w->x, w->y, w->vx, w->vy, tx, ty, maxPower / getMass('S'));
  }
  if (!simThrow)
  {
    *gm_in = *gm;
    return;
  }
  *gm_ans = *gm;
  for (int i = 0; i < sim_c; ++i)
  {
    step(gm, 1);
  }
  *gm_best = *gm;
  *gm = *gm_in;
  int k = 5;
  for (int i = 0; i < k; ++i)
  {
    double alpha = -pi / 2 + pi / k * i;
    if (away)
    {
      alpha = -alpha;
    }
    w->tx = w->x + 10 * cos(alpha);
    w->ty = w->y + 10 * sin(alpha);
    *gm_b = *gm;
    for (int j = 0; j < sim_c; ++j)
    {
      step(gm, 1);
    }
    if (comp1(gm_best, gm, s_id) < 0)
    {
      *gm_best = *gm;
      *gm_ans = *gm_b;
    }
    *gm = *gm_in;
  }
  *gm_in = *gm_ans;
}

void runStupidAi(Gm gm)
{
  if (max_(gm->scores[0], gm->scores[1]) > gmInSc(gm) / 2)
  {
    return;
  }
  assert(gm->ec > sIx);
  E w1 = &gm->es[wIx];
  E w2 = &gm->es[wIx + 1];

  // compute closest snaffles
  setPoint(w1->x, w1->y);
  E s1 = getMin(gm, isS, pointDistSq);
  setPoint(w2->x, w2->y);
  E s2 = getMin(gm, isS, pointDistSq);
  assert(s1 && s2);

  for (int i = 0; i < 2; ++i)
  {
    E w = i ? w2 : w1;
    if (hasSnaffle(w))
    {
      w->action = 'T';
      w->tx = width / 2;
      w->ty = goalY;
      w->power = (int)maxPower;
    }
    else
    {
      E s = i ? s2 : s1;
      assert(s && isS(s));
      w->action = 'M';
      w->power = (int)(!submit && away ? maxThrust * createAsym : maxThrust);
      if (distSq(w, s) < eps)
      {
        w->tx = s->x;
        w->ty = s->y;
        w->power = 0;
      }
      else
      {
        getCorrectThrowLoc(&w->tx, &w->ty, w->x, w->y, w->vx, w->vy, s->x, s->y, maxThrust / getMass('W'));
      }
    }
  }
}

void runBasicAi(Gm gm)
{
  if (max_(gm->scores[0], gm->scores[1]) > gmInSc(gm) / 2)
  {
    return;
  }
  assert(gm->ec > sIx);
  E w1 = &gm->es[wIx];
  E w2 = &gm->es[wIx + 1];

  // compute closest snaffles
  setPoint(w1->x, w1->y);
  E s1 = getMin(gm, isS, pointDistSq);
  setPoint(w2->x, w2->y);
  E s2 = getMin(gm, isS, pointDistSq);
  assert(s1 && s2);

  // handle same snaffle
  if (s1 == s2 && gmSc(gm) >= 2)
  {
    setPoint(w1->x, w1->y);
    exceptId = s1->id;
    xChar = 'S';
    E s1_new = getMin(gm, isXExcept, pointDistSq);
    setPoint(w2->x, w2->y);
    E s2_new = getMin(gm, isXExcept, pointDistSq);
    assert(s1_new && s2_new);
    if (!hasSnaffle(w1) && dist(w1, s1_new) + dist(w2, s2) < dist(w1, s1) + dist(w2, s2_new))
    {
      s1 = s1_new;
    }
    else
    {
      s2 = s2_new;
    }
  }

  // check for snaffle that is left behind
  int n = gmSc(gm);
  E *ss = malloc(sizeof(E) * n);
  for (int i = 0; i < n; ++i)
  {
    ss[i] = &gm->es[sIx + i];
  }
  qsort(ss, n, sizeof(E), snaffleCompar);
  int k = (gmInSc(gm) + 1) / 2 - gm->scores[away];
  assert(k <= n); assert(k > 0);
  E s3 = ss[n - k];
  double a = away ? -1 : 1;
  if (s3->x * a < min_(s1->x * a, s2->x * a) && away)
  {
    if (distSq(w2, s3) < distSq(w1, s3))
    {
      swapE_(w1, w2);
      swapE_(s1, s2);
    }
    if (dist(w1, s1) + dist(s1, s3) > dist(w1, s3) * detourFactor)
    {
      s1 = s3;
    }
  }
  free(ss);

  for (int i = 0; i < 2; ++i)
  {
    E w = i ? w2 : w1;
    if (w->action != '\0')
    {
      continue;
    }
    if (hasSnaffle(w))
    {
      findThrowLoc(gm, w->id);
    }
    else
    {
      E s = i ? s2 : s1;
      assert(s && isS(s));
      w->action = 'M';
      w->power = (int)(!submit && away ? maxThrust * createAsym : maxThrust);
      if (distSq(w, s) < eps)
      {
        w->tx = s->x;
        w->ty = s->y;
        w->power = 0;
      }
      else
      {
        getCorrectThrowLoc(&w->tx, &w->ty, w->x, w->y, w->vx, w->vy, s->x, s->y, maxThrust / getMass('W'));
      }
    }
  }
}

void sim(Gm gm)
{
  step(gm, 1);
}

void runSimAi(Gm gm_in)
{
  forbidAllyActions(gm_in);
  if (!enable_spells)
  {
    runBasicAi(gm_in);
  }
  static GmStruct _gms[4];
  Gm const gm_ans = &_gms[0];
  Gm const gm = &_gms[1];
  Gm const gm_b = &_gms[2];
  Gm const gm_best = &_gms[3];
  if (gm_in->manas[away] >= 20)
  {
    *gm = *gm_in;
    for (int i = 0; i < simC; ++i)
    {
      sim(gm);
    }
    *gm_best = *gm;
    *gm_ans = *gm_in;

    *gm = *gm_in;
    const char *str = "PFA";
    for (int i_str = 0; i_str < strlen(str); ++i_str)
    {
      char c = str[i_str];
      Sp *sp = NULL;
      if (c != 'P')
      {
        sp = getSp(gm, c, away);
        if (gm->tick <= sp->tick)
        {
          continue;
        }
      }
      for (int i_wiz = 0; i_wiz < 2; ++i_wiz)
      {
        E w = &gm->es[wIx + i_wiz];
        if (hasSnaffle(w))
        {
          continue;
        }
        for (int i = 0; i < gm->ec; ++i)
        {
          E e = &gm->es[i];
          if (isW(e) || (isO(e) && c == 'A'))
          {
            continue;
          }
          w->action = c;
          w->power = e->id;
          gm->manas[away] -= getMana(c);
          *gm_b = *gm;
          for (int k = 0; k < simC; ++k)
          {
            sim(gm);
          }
          if (comp(gm_best, gm) < 0)
          {
            *gm_ans = *gm_b;
            *gm_best = *gm;
          }
          *gm = *gm_in;
        }
      }
    }
    *gm_in = *gm_ans;
  }
  runBasicAi(gm_in);
}

void setOrientation()
{
  if (away)
  {
    goalX = 0;
    homeX = width;
    wIx = 2;
    oIx = 0;
  }
  else
  {
    goalX = width;
    homeX = 0;
    wIx = 0;
    oIx = 2;
  }
}

void swapOrientation()
{
  away = !away;
  setOrientation();
}

void runAi(Gm gm)
{
  forbidAllyActions(gm);
  if (stupidAi)
  {
    runStupidAi(gm);
  }
  else
  {
    runSimAi(gm);
  }
}

void runAis(Gm gm)
{
  GmStruct _gm2;
  Gm gm2 = &_gm2;
  *gm2 = *gm;
  runAi(gm2);
  swapOrientation();
  runAi(gm);
  swapOrientation();
  for (int i = 0; i < 2; ++i)
  {
    E w2 = &gm2->es[wIx + i];
    E w = &gm->es[wIx + i];
    w->action = w2->action;
    w->tx = w2->tx;
    w->ty = w2->ty;
    w->power = w2->power;
  }
}

void battle()
{
  if (!lcl)
  {
    scanf("%d", &away);
  }
  setOrientation();
  static GmStruct gms[2];
  Gm gm_r = &gms[0]; // received game
  Gm gm = &gms[1];

  readGm(gm);
  while (1)
  {
    runAi(gm);
    if (!lcl && !submit)
    {
      for (int i = 0; i < 2; ++i)
      {
        E w = &gm->es[wIx + i];
        fprintf(stderr, "%c %d %d %d\n", w->action, rnd(w->tx), rnd(w->ty), w->power);
      }
    }
    printActions(gm);
    readGm(gm_r);
    step(gm, 0);
    fixGm(gm, gm_r);
  }
}

#ifdef LCL

void render()
{
  Gm gm;
  for (int l = 0; l < 4; ++l)
  {
  	if ((l % 2) == 0 && (gmsB == 0 || hideB))
  	{
  		continue;
  	}
    gm = (l % 2) == 0 ? &gmsB[gmTick] : liveGm;
    if ((l % 2) == 1 && showExtra)
    {
      gm = &gmsX[gmTick];
    }
    for (int i = -4; i < gm->ec; ++i)
    {
      E e = i < 0 ? &poles[4 + i] : &gm->es[i];
      if ((l < 2) != isWiz(e))
      {
        continue;
      }
      double x = e->x / zoom;
      double y = e->y / zoom;
      double r = getRadius(e->c) / zoom;
      double al = 0;
      double red = 0, g = 0, b = 0;
      switch (e->c)
      {
      case 'S': red = 0; g = 0; b = 0.7; break;
      case 'W': red = 0.7; g = 0.7; b = 0; break;
      case 'O': red = 0.9; g = 0; b = 0; break;
      }
      double a = (l % 2) == 0 ? 0.4 : 1;
      int k = 2;
      if (isWiz(e))
      {
        int team = getTeam(e);
        if ((gm->tick <= gm->spells[2 + team].tick && gm->spells[2 + team].id == e->id))
        {
          k = 1;
        }
        else if ((gm->tick <= gm->spells[4 + team].tick && gm->spells[4 + team].id == e->id))
        {
          k = 8;
        }
      }
      if (e->c != 'P' && (gm->petrifieds[0] == e->id || gm->petrifieds[1] == e->id))
      {
        red = 0; b = 0; g = 0.8;
      }
      for (int j = 0; j < k; ++j)
      {
        S2D_DrawTriangle(
          (GLfloat)(x + cos(al) * r), (GLfloat)(y - sin(al) * r), (GLfloat)red, (GLfloat)g, (GLfloat)b, (GLfloat)a,
          (GLfloat)(x + cos(al + 2 * pi / 3) * r), (GLfloat)(y - sin(al + 2 * pi / 3) * r), (GLfloat)red, (GLfloat)g, (GLfloat)b, (GLfloat)a,
          (GLfloat)(x + cos(al - 2 * pi / 3) * r), (GLfloat)(y - sin(al - 2 * pi / 3) * r), (GLfloat)red, (GLfloat)g, (GLfloat)b, (GLfloat)a
        );
        al += pi * 2 / 3 / k;
      }
    }
  }
}

void moveCatchedSnaffles(Gm gm)
{
  for (int i = sIx; i < gm->ec; ++i)
  {
    E s = &gm->es[i];
    int id = s->wizId;
    if (id >= 0)
    {
      E w = getE(gm, id);
      assert(isWiz(w));
      s->x = w->x;
      s->y = w->y;
    }
  }
}

void onNewTick()
{
  printf("%d", gmTick * 2);
  if (showDiff && diff(&gmsF[gmTick], &gmsB[gmTick])) { printf(": diff"); }
  printf("\n");
}

void update()
{
  static double t0;
  double t1 = getElapsed();
  double t = t1 - t0;
  t0 = t1;
  if (t == t1 || t <= 0 || !live) return;
  Gm gm = liveGm;
  double dt = t * speed;
  int tick_before = gmTick;
  while (gm->time + dt > 1)
  {
    dt -= (1 - gm->time);
    gmTick = (gmTick + 1) % gmC;
    *gm = gmsF[gmTick];
  }
	if (loopTick >= 0 && gmTick >= loopTick + loopWidth)
	{
		gmTick = loopTick;
		*gm = gmsF[gmTick];
    onNewTick();
		return;
	}
  if (gmTick != tick_before)
  {
  	onNewTick();
  }
  if (gmTick < gmC - 1)
  {
    if (gm->time == 0)
    {
      beginStep(gm, 0);
    }
    simGm(gm, dt);
    moveCatchedSnaffles(gm);
  }
  gm->time += dt;
}

void on_key(const char *key)
{
  //printf("%s\n", key);
  if (!strcmp(key, "Escape"))
  {
    S2D_Close(window);
  }
  if (!strcmp(key, "Right"))
  {
    gmTick = (gmTick + 1) % gmC;
    *liveGm = gmsF[gmTick];
    onNewTick();
  }
  if (!strcmp(key, "Left"))
  {
    gmTick = (gmTick + gmC - 1) % gmC;
    *liveGm = gmsF[gmTick];
    onNewTick();
  }
  if (!strcmp(key, "D")) // show difference
  {
    if (diff(&gmsF[gmTick], &gmsB[gmTick])) { printDiff(&gmsF[gmTick], &gmsB[gmTick]); }
  }
  if (!strcmp(key, "Space"))
  {
    live = !live;
  }
  if (!strcmp(key, "E"))
  {
    showExtra = !showExtra;
  }
  if (!strcmp(key, "B")) // background
  {
    hideB = !hideB;
  }
  if (!strcmp(key, "["))
  {
    speed /= speedUp;
  }
  if (!strcmp(key, "]"))
  {
    speed *= speedUp;
  }
  if (!strcmp(key, "-"))
  {
    if (loopWidth > 1)
    {
    	loopWidth--;
    }
  }
  if (!strcmp(key, "="))
  {
    loopWidth++;
  }
  if (!strcmp(key, "L")) // toggle loop
  {
    if (loopTick >= 0)
    {
    	loopTick = -1;
    }
    else
    {
    	loopTick = gmTick;
    }
  }
}

void runLcl()
{
  setOrientation();
  scanf("%d", &gmC);
  gmC = (gmC - 1) / 2;
  assert(gmC > 0);
  assert(gmC <= maxGmC);
  if (lclBattle)
  {
    //battle();
  }
  else
  {
  	GmStruct _gm;
  	Gm gm = &_gm;
  	int diffs[maxGmC];
  	int difc = 0;
    for (int i = 0; i < gmC; ++i)
    {
    	gmTick = i;
		  readGm(gm);
    	if (i == 0)
    	{
		  	gmsR[0] = *gm;
		  	gmsS[0] = *gm;
    	}
    	else
    	{
    		gmsR[i] = gmsR[i - 1];
    		updateSpells(&gmsR[i]);
        fixGm(&gmsR[i], gm);
        copyActions(&gmsR[i], gm);
        copyActions(&gmsS[i], gm);
    	}
	  	if (showDiff)
	  	{
	  		if (diff(&gmsR[i], &gmsS[i]))
	      {
	        diffs[difc++] = i * 2;
	      }
	  	}
      if (i + 1 < gmC)
      {
        gmsS[i + 1] = gmsR[i];
        step(&gmsS[i + 1], 1);
      }
    }
  	if (showDiff)
  	{
  		printf("diffs: ");
  		for (int i = 0; i < difc; ++i)
  		{
  			printf("%d, ", diffs[i]);
  		}
  		printf("\n");
  	}
    gmsB = gmsS;
    gmsF = gmsR;
    gmTick = 0;
  }
  window = S2D_CreateWindow(
    "Fantastic Bits", (int)(width / zoom), (int)(height / zoom), update, render, 0
  );
  window->background.r = 1.0;
  window->background.g = 1.0;
  window->background.b = 1.0;
  window->on_key = on_key;
  gmTick = (startFrame / 2) % gmC;
  if (startLoop)
  {
  	loopTick = gmTick;
  	loopWidth = startLoop;
  }
  *liveGm = gmsF[gmTick];
  startUp = 0;
  S2D_Show(window);
  S2D_FreeWindow(window);
}

#endif

int main(int argc, char *argv[])
{
  start = clock();
  mkPole(&poles[0], 0, goalY - goalR);
  mkPole(&poles[1], 0, goalY + goalR);
  mkPole(&poles[2], width, goalY - goalR);
  mkPole(&poles[3], width, goalY + goalR);
  if (lcl)
  {
    runLcl();
  }
  else
  {
    battle();
  }
  return 0;
}

