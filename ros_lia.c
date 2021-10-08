/*
rosalind
Independent Alleles
[LIA]
 */

#include <stdio.h>
#include <string.h>

int memo[21][21];

int k,N;

int binomial(int n,int r)
{ int ret;
  if((ret=memo[n][r])>0)
    return ret;
  if(r==n || r==0)
    ret = 1;
  else if (r==n-1 || r==1)
    ret = n;
  else
    ret=binomial(n-1,r-1)+binomial(n-1,r);

  memo[n][r]=ret;
  //printf("%d %d:%d\n",n,r,ret);
  return ret;
}

int expt2(int n)
{
  if(n <= 0)
    return(1);
  else
    return 2*expt2(n-1);
}
double expt(double x,int n)
{
  if(n <= 0)
    return(1.0);
  else
    return x*expt(x,n-1);
}
  

double calc_bino_ge(int n,int k,double p)
{ double ret=0.0;
  
  printf("arg=%d %d\n",n,k);

  for(int i=k;i<=n;i++)
    {
    ret += binomial(n,i)*expt(p,i)*expt(1.0-p,n-i);
    }
  return(ret);
}


int main()
{
  scanf("%d %d",&N,&k);
 
  printf("%lf\n",calc_bino_ge(expt2(N),k,0.25));

  return(0);
}
