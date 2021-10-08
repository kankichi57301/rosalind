/*
rosalind
Mendel's First Law
[IPRB]
 */

#include <stdio.h>
#include <string.h>


int main()
{
  double ret;
  int k,m,n,t,i;
  double num,den;
  
  scanf("%d %d %d",&k,&m,&n);

  t = k+m+n;
  den = t*(t-1)/2.0;
  printf("[%lf][%d]\n",den,t);
  
  num=0;
  for(i=0;i<k;i++)
    num += (t-1-i); //denominant vs any
  printf("num=%lf den=%lf[%d][%d]\n",num,den,k,t);
  num += m*(m-1)/2 * (double)(3.0/4);
  printf("2)[%lf]\n",m*(m-1)/2*(3.0/4));
  num += m*n       * (double)(1.0/2);
  printf("3)[%lf]\n",m*n*(1.0/2));
  
  ret = num/den;

  printf("num=%lf den=%lf\n",num,den);
  printf("%lf\n",ret);
  
  return(0);
}
