/*
Rosalind 

[LAFF]

 */
#include <stdio.h>
#include <string.h>
#include "b62.h"
#define max(x,y) (((x)>(y))?(x):(y))
//#define max3(x,y,z) max(x,(max(y,z)))
#define max4(x,y,z,w) max(max(x,y),(max(z,w)))
//#define DUMP  1
//#define DEBUG 1
char buf[80];
short dp[10001][10001];
char  tb[10001][10001];
char dna [2][10001];
int len1,len2;
int outlen;
int max_score,max_x,max_y;
char res[2][20001];
char title1[80];
char title2[80];

int gap_pen_open= -11;
int gap_pen_ext= -1;


void init_dp()
{
  int i;
  memset(dp,0,sizeof(dp));
  memset(tb,0,sizeof(tb));
}
int max_index(int v1,int v2,int v3,int v4)
{ 
  if(v1 >= v2 && v1 >=v3 && v1 >= v4)
    return 0;
  if(v2 >= v1 && v2 >=v3 && v2 >= v4)
    return 1;
  if(v3 >= v1 && v3 >=v2 && v3 >= v4)
    return 2;
  if(v4 >= v1 && v4 >=v2 && v4 >= v3)
    return 3;
}
void fill_dp(char str1[],char str2[],int len1,int len2)
{
  int x,y,c,v1,v2,v3,g_p_x,g_p_y,val;
  for(y=1;y<=len2;y++)
    {
#ifdef COUNTSTATE      
      if((y % 1000)==0)
	printf("%d\n",y);
#endif
      for(x=1;x<=len1;x++)
      {
	g_p_x = (tb[x-1][y]==0)?gap_pen_ext:gap_pen_open;
	g_p_y = (tb[x][y-1]==1)?gap_pen_ext:gap_pen_open;

	c=protein_score(str1[x-1],str2[y-1]);
	v1 = dp[x-1][y] + g_p_x;
	v2 = dp[x][y-1] + g_p_y;
	v3 = dp[x-1][y-1] + c;
	dp[x][y]=val=max4(v1,v2,v3,0);
	tb[x][y]=max_index(v1,v2,v3,0);
	if(val> max_score)
	  {
	    max_score=val;
	    max_x = x;
	    max_y = y;
	  }
      }
    }
}

void output_result()
{ int i,j;
  for(i=0;i<2;i++)
    {
      for(j=outlen-1;j>=0;j--)
	putchar(res[i][j]);
      putchar('\n');
    }
}


void fukugen(int x,int y)
{ char dir,match;
  if(x==0 || y==0)
    return;
  dir=tb[x][y];
    
  switch(dir)
  {
  case 0:
      res[0][outlen]=dna[0][x-1];
      res[1][outlen]='-';
      outlen++;
      fukugen(x-1,y);
      break;
  case 1:
      res[0][outlen]='-';
      res[1][outlen]=dna[1][y-1];
      outlen++;
      fukugen(x,y-1);
      break;
  case 2:
      res[0][outlen]=dna[0][x-1];
      res[1][outlen]=dna[1][y-1];
      outlen++;
      fukugen(x-1,y-1);
      break;
  case 3:
    return;
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
  init_dp();
  max_score=0;
  fill_dp(str1,str2,len1,len2);

  //printf("max score=%d|" ,max_score);
  //printf("max x=%d|" ,max_x);
  //printf("max y=%d\n" ,max_y);
#ifdef DUMP
  dump_dp(str1,str2);
#endif
  //dump_tb(str1,str2);
  
  outlen=0;
  
  fukugen(max_x,max_y);
  //printf("outlen=%d\n" ,outlen);
  printf("%d\n",max_score);
  output_result();
}

int main()
{
  scanf("%s",title1);
  while(1)
  {
    scanf("%s",buf);
    if(buf[0]=='>')
      break;
    strcat(&dna[0][0],buf);
  }
  while(EOF!=scanf("%s",buf))
  {
    strcat(&dna[1][0],buf);
  }
  scanf("%s",&dna[1][0]);
  len1=strlen(&dna[0][0]);
  len2=strlen(&dna[1][0]);
  //printf("len1=%d len2=%d\n",len1,len2);
  //printf("dna1=%s\n",&dna[0][0]);
  //printf("dna2=%s\n",&dna[1][0]);
  solve(&dna[0][0],&dna[1][0]);
  return(0);
}
