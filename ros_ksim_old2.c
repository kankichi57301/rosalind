#include <stdio.h>
#include <string.h>
#define min(x,y) (((x)<(y))?(x):(y))
#define min3(x,y,z) min(x,(min(y,z)))
//#define DUMP  1
//#define DEBUG 1
/*
rosalind
[KSIM]
 */
short dp[5001][50001];
char  tb[5001][50001];
char  hash[500000];
char motif[5001];
char dna  [50001];
int len1,len2;
int k;
int gap_pen=1;

void init_dp()
{
  int i;
  //memset(dp,0,sizeof(dp));
  //memset(tb,0,sizeof(tb));
  for(i=1;i<=len1;i++)
    tb[i][0]=1;
  for(i=1;i<=len2;i++)
    tb[0][i]=2;
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
extern int hash_val(int,int);
extern int hash_exist(int,int);
extern void set_hash(int,int);
void output_result(int stlist[],int y,int cnt)
{ int i,s,len;
  for(i=0;i<cnt;i++)
    {
      s=stlist[i]+1;
      len=y-stlist[i];
      //printf("y,len,hash=%d,%d,%d\n",s,len,hash_val(s,len));
      if(hash_exist(s,len)==0)
	{
	  printf("%d %d\n",s,len);
	  set_hash(s,len);
	}
    }
}
int st_l[100];
int tb_cnt;
void traceback(char str1[],char str2[],int x,int y,int k0)
{ char dir,match;
  dir=tb[x][y];
#ifdef DEBUG3
  printf(">%d,%d[%d][%d]\n",x,y,k0,dir);
#endif
  if(x <= k0)
    { //printf("*\n");
      st_l[tb_cnt]=y;
      tb_cnt++;
    }
  if(((x ==0) && (y==0))||(k0 < 0)) 
     return;
  if((dir&1)>0)
    traceback(str1,str2,x-1,y,k0-1);
  if((dir&2)>0)
    traceback(str1,str2,x,y-1,k0-1);
  if((dir&4)>0)
  {
       match=(str1[x-1]==str2[y-1])?0:1;
       traceback(str1,str2,x-1,y-1,k0-match);
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
void dump_tb(char str1[],char str2[])
{ int i,x,y;
  printf("--------------\n");
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
	printf("\x1b[43m%d \x1b[49m",tb[x][y]);
      else
	printf("%d ",tb[x][y]);
    }
    putchar('\n');
  }
}
void solve(char str1[],char str2[])
{
  int x,y,sc;
  len1=strlen(motif);
  len2=strlen(dna);
  init_dp();
  fill_dp(str1,str2,len1,len2);
#ifdef DUMP
  dump_dp(str1,str2);
  //dump_tb(str1,str2);
#endif
  for(y=1;y<=len2;y++)
  {
    for(x=len1;x>=(len1-(k+1));x--)
      {
	sc=dp[x][y];
	if(sc + (len1 - x)<= k)
	{
#ifdef DEBUG2	  
	  printf("x=%d y=%d sc=%d\n",x,y,sc);
#endif
	  tb_cnt=0;
	  traceback(str1,str2,x,y,(k- (len1-x)));
#ifdef DEBUG2	  
	  printf("cnt=%d\n",tb_cnt);
	  for(int i=0;i<tb_cnt;i++)
	    printf(":%d\n",st_l[i]);
#endif	  
	  if(tb_cnt > 0)
	    output_result(st_l,y,tb_cnt);
	  
	}
      }
  }
  
}

int hash_val(int s,int len)
{
  return (s-1)*100+(50+len-len1);
}
void set_hash(int s,int len)
{
  hash[hash_val(s,len)]=1;
}
int hash_exist(int s,int len) 
{
  return hash[hash_val(s,len)];
}



int main()
{
  scanf("%d",&k);
  scanf("%s",motif);
  scanf("%s",dna);
  //printf("len1=%d len2=%d\n",len1,len2);
  solve(motif,dna);
  return(0);
}
