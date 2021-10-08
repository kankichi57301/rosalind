/*
rosalind
Calculating Expected Offspring
[IEV] 
2021/10/08 @kankichi57301
 */

#include <stdio.h>

int main()
{
  double ret;
  int    a,b,c,d,e,f;
  
  scanf("%d %d %d %d %d %d",&a,&b,&c,&d,&e,&f);

  
  ret = 2.0*(a+b+c)+1.5*d+e;

  
  printf("%.1lf\n",ret);
  
  return(0);
}
