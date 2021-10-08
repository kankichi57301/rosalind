/*
rosalind
Rabbits and Recurrence Relations
[FIB]
 */

#include <stdio.h>
#include <string.h>

int n,k;
char dna[1001];
long long memo[50];

long long rabbit(int month,int offspring)
{ long long gen1,gen2,ret;

  if ((ret=memo[month])>0L)
    return(ret);
  
  if(month==1)
    ret= 1;
  else if (month==2)
    ret= offspring;
  else
    {
      gen1 = rabbit (month-1,offspring);
      gen2 = rabbit (month-2,offspring);
      if(month <= 4)
	ret= gen1+gen2;
      else
	ret= gen1+gen2*offspring;
    }
  memo[month]=ret;
  return(ret);
}
int main()
{
  memset(memo,0,sizeof(memo));
  scanf("%d %d",&n,&k);
  printf("%lld",rabbit(n,k));
  return(0);
}
