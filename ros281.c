#include <stdio.h>
#include <string.h>
/*
rosalind
Longest Increasing Subsequence
 */
int s[10001];
int r[10001];
int t[10001];
int u[10001];
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
  int len,next,i,w;
  r[0]=s[0];
  len=1;
  for(i=1;i<n;i++)
  {
    next=s[i];
    if(r[len-1]<next)
    {
      r[len]=next;
      t[i]=len;
      len ++;
    }
    else
    {
	r[w=find_GT_pos(r,len,next)]=next;
	t[i]=w;
    }
  }
  return(len);
}
int solve2()
{
  int len,next,i,w;
  r[0]=s[0];
  len=1;
  for(i=1;i<n;i++)
  {
    next=s[i];
    if(r[len-1]>next)
    {
      r[len]=next;
      t[i]=len;
      len ++;
    }
    else
    {
      r[w=find_LT_pos(r,len,next)]=next;
      t[i]=w;
    }
  }
  return(len);
}


int main()
{
  int i,ret,m,j;
  scanf("%d",&n);
  for(i=0;i<n;i++)
    scanf("%d",&s[i]);

  printf("n=%d\n",n);
    
  memset(r,0,sizeof(r));
  memset(t,0,sizeof(t));
  memset(u,0,sizeof(u));

  
  ret=solve();

  //printf("\nlis len=%d\n",ret);
  //for(i=0;i<ret;i++)
  //  printf("%d ",r[i]);
  //putchar('\n');
  //printf("\nlis len=%d\n",ret);
  //LIS‚Ì•œŒ³
  
  m=ret-1;
  
  for(i=n-1;i>=0;i--)
    if(t[i]==m)
      {
	u[m]=s[i];
	m--;
      }

  for(i=0;i<ret;i++)
    printf("%d ",u[i]);
  putchar('\n');
  
  
  memset(r,0,sizeof(r));
  memset(t,0,sizeof(t));
  memset(u,0,sizeof(u));

  ret=solve2();

  //printf("\nlds len=%d\n",ret);

  /*
  for(i=0;i<ret;i++)
    printf("%d ",r[i]);
  putchar('\n');
  for(i=0;i<n;i++)
    printf("%d ",t[i]);
  */

  
  m=ret-1;

  for(i=n-1;i>=0;i--)
    if(t[i]==m)
      {
	u[m]=s[i];
	m--;
      }

  for(i=0;i<ret;i++)
    printf("%d ",u[i]);
  
  
  
  return(0);
}
