#include <stdio.h>
#include <stdlib.h>

int mult (int a, int b);
int raise (int a, int b);

int main()
{
  int b, e;

  printf ("Enter non-negative base and exponent: ");
  scanf ("%d %d", &b, &e);
  if (b == 0 && e == 0 ){
    printf ("Error: 0 ^ 0.\n");
    exit (-1);
  }
  printf ("%d ^ %d = %d\n", b, e, raise (b, e));
}

int mult (int a, int b)
{
  if (a == 0)
  {
    return 0;
  }
  return b + mult (a-1, b);
}

int raise (int a, int b)
{
  if (b == 0)
  {
    return 1;
  }

  if ((b & 0x1) == 0x1)
  {
    return mult (a, raise (a, b-1));
  }

  int temp = raise (a, b/2);
  return mult (temp, temp);
}