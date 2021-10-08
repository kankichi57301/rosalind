/*
rosalind
Counting Point Mutations
{HAMM]
 */

#include <stdio.h>
#include <string.h>

char dna[2][1001];
int main()
{
  char  *p,*q;
  int   cnt;
  
  scanf("%s",&dna[0][0]);
  scanf("%s",&dna[1][0]);
  for(p=&dna[0][0],q=&dna[1][0],cnt=0;*p;p++,q++)
    {
      if(*p != *q)
	cnt++;
    }
  printf("%d\n",cnt);
  
  return(0);
}
