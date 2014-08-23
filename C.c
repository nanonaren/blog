#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <time.h>

double computeR(double* m,int row)
{
  double total = 0;
  int i,j;

  for (i=0; i<1000; i++)
  {
    int x = INT_MAX;
    for (j=0; j<1000; j++)
    {
      x = x < (*m) ? x : (*m);
      m++;
    }
    total += x;
  }
  return total;
}

double computeC(double* m,int row)
{
  double total = 0;
  int i,j;

  for (i=0; i<1000; i++)
  {
    int x = INT_MAX;
    for (j=i; j<row*1000+i; j=j+row)
    {
      int y = m[j];
      x = x < y ? x : y;
    }
    total += x;
  }
  return total;
}


int main ()
{
  double m[1000*1000];
  int i,j;
  double* p = m;

  for (i=0; i<1000; i++)
  {
    for (j=0; j<1000; j++)
    {
      *p = rand();
      p++;
    }
  }

  clock_t t;
  double res=0;

  t = clock();

  for (i=0; i<1000; i++)
  {
    res += computeR(m,1000);
  }
  printf ("%f\n", res);

  t = clock() - t;

  printf ("%f milliseconds\n",1000 * (((float)t)/CLOCKS_PER_SEC/1000));
}
