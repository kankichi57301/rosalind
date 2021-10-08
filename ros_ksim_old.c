#include <stdio.h>
#include <string.h>
#define min(x,y) (((x)<(y))?(x):(y))
#define min3(x,y,z) min(x,(min(y,z)))
#define DUMP 1
/*
rosalind
[KSIM]
 */
short dp[5001][50001];
char  tb[5001][50001];
char motif[5001];
char dna  [50001];
int len1,len2;
int k;
int gap_pen=1;

void init_dp()
{
  int i;
  memset(dp,0,sizeof(dp));
  memset(tb,0,sizeof(tb));
}
int comp3_bit(int v1,int v2,int v3)
{
  if(v1==v2 && v2 ==v3)
    return 7;
  if(v1==v2 && v1 < v3)
    return 3;
  if(v2==v3 && v2 < v1)
    return 6;
  if(v3==v1 && v3 < v2)
    return 5;
  if(v1 < v2 && v1 < v3)
    return 1;
  if(v2 < v3 && v2 < v1)
    return 2;
  if(v3 < v1 && v3 < v2)
    return 4;
}
void fill_dp(char str1[],char str2[],int len1,int len2)
{
  int x,y,c,v1,v2,v3;
  for(y=1;y<=len2;y++)
    {
      if((y % 1000)==0)
	printf("%d\n",y);
      for(x=1;x<=len1;x++)
      {
	c=(str1[x-1]==str2[y-1])?0:1;
	v1 = dp[x-1][y] + gap_pen;
	v2 = dp[x][y-1] + gap_pen;
	v3 = dp[x-1][y-1] + c;
	dp[x][y]=min3(v1,v2,v3);
	tb[x][y]=comp3_bit(v1,v2,v3);
      }
    }
}

void output_result(int stlist[],int y,int cnt)
{ int i;
  for(i=0;i<cnt;i++)
    printf("%d %d\n",stlist[i]+1,y-stlist[i]);
}
int traceback(char str1[],char str2[],int x,int y,int k0,int stl[],int cnt)
{ char dir,match;
  dir=tb[x][y];
#ifdef DEBUG
  (printf ">%d,%d[%d][%d]\n",x,y,k0,dir);
#endif
  if(x <= k0)
  {   stl[cnt]=y;
      cnt++;
  }
  if(((x ==0) && (y==0))||(k < 0))
     return cnt;
  if((dir|1)>0)
    traceback(str1,str2,x-1,y,k-1,stl,cnt);
  if((dir|2)>0)
    traceback(str1,str2,x,y-1,k-1,stl,cnt);
  if((dir|4)>0)
     {
       match=(str1[x-1]==str2[y-1])?0:1;
       traceback(str1,str2,x-1,y-1,k-match,stl,cnt);
     }
}
void dump_dp(char str1[],char str2[])
{ int i,x,y;
  printf("- - ");
  for(i=0;i<len1;i++)
    printf("%c ",str1[i]);
  putchar('\n');
  for(y=0;y<=len2;y++)
  {
    if(y==0)
      printf("- ");
    else
      printf("%c ",str2[y-1]);
    for(x=0;x<=len1;x++)
    {
      if(x>0 && y>0 && (str1[x-1]==str2[y-1]))
	printf("\x1b[42m%d \x1b[49m",dp[x][y]);
      else
	printf("%d ",dp[x][y]);
    }
    putchar('\n');
  }
}

void solve(char str1[],char str2[])
{
  int x,y,sc,cnt,st_l[50];
  len1=strlen(motif);
  len2=strlen(dna);
  init_dp();
  fill_dp(str1,str2,len1,len2);
  dump_dp(str1,str2);
  /*
  for(y=1;y<=len2;y++)
  {
    for(x=len1;x>0;x--)
      {
	sc=dp[x][y];
	if(sc <= k)
	{
	  cnt=traceback(str1,str2,x,y,(k- (len1-x)),st_l,0);
	   if(cnt > 0)
	     output_result(st_l,y,cnt);
	}
      }
  }
  */
}

int main()
{
  scanf("%d",&k);
  scanf("%s",motif);
  scanf("%s",dna);
  printf("len1=%d len2=%d\n",len1,len2);
  solve(motif,dna);
  return(0);
}
