#include <stdio.h>
#include <stdlib.h>

#define SIZE 100

int n; //num of items in array
int list[SIZE]; //the array

int i, j; //loop control vars
int min_pos; //position of min element during source
int temp; //temporary var

int main()
{
  printf ("Enter number of elements in array: ");
  scanf ("%d", &n);

  printf ("Enter array elements, one per line:\n");

  for (i=0; i < n; i++)
  {
    scanf ("%d", &list[i]);
  }

  for (i = 0; i < n - 1; i++)
  {
    min_pos = i;
    for (j = i + 1; j < n; j++)
    {
      if (list[j] < list[min_pos])
      {
        min_pos = j;
      }
    }
      temp = list[i];
      list[i] = list[min_pos];
      list[min_pos] = temp;
    
  }

  printf ("\n");
  for(i = 0; i < n; i++)
  {
    printf ("%d\n", list[i]);
  }

  exit (0);
}