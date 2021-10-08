#include <stdio.h>
#include <string.h>
/*
rosalind
Longest Increasing Subsequence
[LGIS]
 */
int s[10001];
int r[10001];
int t[10001];
int n;


int find_GT_pos(int *seq,int len,int n)
{
  int i,center;
  if(len < 4)
    {
      for(i=0;i<len;i++)
	if(seq[i]>n)
	  return(i);
    }
  else
    {
      center=len/2;
      if(seq[center] > n)
	return find_GT_pos(seq,center, n);
      else
	return center + find_GT_pos(seq+center,center, n);
    }
}
int find_LT_pos(int *seq,int len,int n)
{
  int i,center;
  if(len < 4)
    {
      for(i=0;i<len;i++)
	if(seq[i]<n)
	  return(i);
    }
  else
    {
      center=len/2;
      if(seq[center] < n)
	return find_LT_pos(seq,center, n);
      else
	return center + find_LT_pos(seq+center,center, n);
    }
}


int solve()
{
  int len,next,i;
  r[0]=s[0];
  len=1;
  for(i=1;i<n;i++)
  {
    next=s[i];
    if(r[len-1]<next)
    {
      r[len]=next;
      len ++;
    }
    else
      r[find_GT_pos(r,len,next)]=next;
  }
  return(len);
}
int solve2()
{
  int len,next,i;
  r[0]=s[0];
  len=1;
  for(i=1;i<n;i++)
  {
    next=s[i];
    if(r[len-1]>next)
    {
      r[len]=next;
      len ++;
    }
    else
      r[find_LT_pos(r,len,next)]=next;
  }
  return(len);
}


int main()
{
  int i,ret,m;
  scanf("%d",&n);
  for(i=0;i<n;i++)
    scanf("%d",&s[i]);

  printf("n=%d\n",n);

  
  ret=solve();
  printf("ret=%d\n",ret);
  for(i=0;i<ret;i++)
    printf("%d ",r[i]);
  putchar('\n');
  printf("\nlcs len=%d\n",ret);
  //LIS‚Ì•œŒ³

  m=ret-1;
  for(i=n-1;i>=0;i--)
    if(m==ret-1)
      {
	if(r[m] <= s[i])
	  {
	    t[m]=s[i];
	    m --;
	  }
      }
    else
      {
	if(r[m] <= s[i] && s[i] < r[m+1])
	  {
	    t[m]=s[i];
	    m --;
	  }
      }


  for(i=0;i<ret;i++)
  printf("%d ",t[i]);

  memset(r,0,sizeof(r));
  ret=solve2();
  m=ret-1;
  for(i=n-1;i>=0;i--)
    if(m==ret-1)
      {
	if(r[m] >= s[i])
	  {
	    t[m]=s[i];
	    m --;
	  }
      }
    else
      {
	if(r[m] >= s[i] && s[i] > r[m+1])
	  {
	    t[m]=s[i];
	    m --;
	  }
      }
  printf("\n");
  for(i=0;i<ret;i++)
  printf("%d ",t[i]);
  
  return(0);
}
