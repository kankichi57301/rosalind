/*
rosalind
Mortal Fibonacci Rabbits
[FIBD]
 */

#include <stdio.h>
#include <string.h>
#define min(x,y) (((x)<(y))?(x):(y))

int n,m,lifespan;
long long rabbits[100][20];

long long calc_rabbit(int month,int age)
{ long long ret,sum;
  int i;

  if((ret=rabbits[month][age])>0)
    return ret;
  if(month == 1)
  {
    if(age==0)
      ret=1;
    else
      ret=0;
  }
  else if(month == 2)
  {
    if(age==0)
      ret=0;
    else if(age==1)
      ret=1;
    else
      ret=0;
  }
  else if(age>0)
    ret=calc_rabbit(month-1,age-1);
  else
    for(i=1,ret=0;i<min(lifespan,month-1);i++)
      ret += calc_rabbit(month-1,i);
  rabbits[month][age]=ret;
  return(ret);
}
long long  calc_rabbit_all(int month)
{
  int i;
  long long ret,r;
  
  for(i=0,ret=0;i<lifespan;i++)
    {
      r = calc_rabbit(month,i);
      ret+=r;
    }
  return(ret);
}

int main()
{
  memset(rabbits,0,sizeof(rabbits));

  lifespan=16;
  
  scanf("%d %d",&n,&lifespan);
  printf("%lld\n",calc_rabbit_all(n));
  
  return(0);
}
