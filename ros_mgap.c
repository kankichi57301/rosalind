/*
Rosalind
Maximizing the Gap Symbols of an Optimal Alignment
racketÇæÇ∆íxÇ¢âΩÇ≈ÇæÇÎÇ§
[MGAP]
2021/03/04 AC
2021/10/21Å@AC
 */
#include <stdio.h>
#include <string.h>

#define GAP '-'
char name[2][100];
char dna[2][5001];
char aligned[2][10001];
int  dp[5001][5001];
char tb[5001][5001];
char buf[200];
int gap_pen = -1;
int match = 1;
int unmatch = -2;
int result;

void strcatchar(char str[],char c)
{
  char buf[2];
  buf[0]=c;
  buf[1]='\0';
  strcat(str,buf);
}
void fukugen(int len1,int len2)
{ char d,c,c1,c2;
  if(len1==0 && len2 ==0)
    return;
  d=tb[len1][len2];
  switch(d)
    {
    case 0:
      c = dna[0][len1-1];
      strcatchar(&aligned[0][0],c);
      strcatchar(&aligned[1][0],GAP);
      fukugen(len1-1,len2);
      break;
    case 1:
      c = dna[1][len2-1];
      strcatchar(&aligned[0][0],GAP);
      strcatchar(&aligned[1][0],c);
      fukugen(len1,len2-1);
      break;
    case 2:
      c1 = dna[0][len1-1];
      c2 = dna[1][len2-1];
      strcatchar(&aligned[0][0],c1);
      strcatchar(&aligned[1][0],c2);
      fukugen(len1-1,len2-1);
      break;
      
    }
}
void count_gap(int len1,int len2)
{ char d,c,c1,c2;
  int ret=0;
  if(len1==0 && len2 ==0)
    return;
  d=tb[len1][len2];
  switch(d)
    {
    case 0:
      c = dna[0][len1-1];
      strcatchar(&aligned[0][0],c);
      strcatchar(&aligned[1][0],GAP);
      result++;
      count_gap(len1-1,len2);
      break;
    case 1:
      c = dna[1][len2-1];
      strcatchar(&aligned[0][0],GAP);
      strcatchar(&aligned[1][0],c);
      result++;
      count_gap(len1,len2-1);
      break;
    case 2:
      c1 = dna[0][len1-1];
      c2 = dna[1][len2-1];
      strcatchar(&aligned[0][0],c1);
      strcatchar(&aligned[1][0],c2);
      count_gap(len1-1,len2-1);
      break;
    }
}

void test01()
{
  char c;
  int i=0;
  while((c=dna[0][i++]) &&(i<20) )
    putchar(c);
  putchar('\n');
  i=0;
  while((c=dna[1][i++]) && (i<20) )
    putchar(c);
  putchar('\n');
}
void init_dp(int len1,int len2)
{
  int i;
  dp[0][0]=0;
  for(i=1;i<=len1;i++)
    dp[i][0]=i*gap_pen;
  for(i=1;i<=len2;i++)
    dp[0][i]=i*gap_pen;
}
void fill_dp(int len1,int len2)
{
  int i,j,v1,v2,v3,v,d;

  for(i=1;i<=len1;i++)
  {
    //if((i % 100) == 0)
      //printf("%d\n",i);
    for(j=1;j<=len2;j++)
      {
	v1=dp[i-1][j] + gap_pen;
	v2=dp[i][j-1] + gap_pen;
	v3=dp[i-1][j-1] + ((dna[0][i-1]==dna[1][j-1])?match:unmatch);
	if(v1<v2)
	  {
	    if(v2<v3)
	      {
		v = v3;
		d = 2;
	      }
	    else
	      {
		v =v2;
		d = 1;
	      }
	  }
	else
	  {
	    if(v1<v3)
	      {
		v = v3;
		d = 2;
	      }
	    else
	      {
		v = v1;
		d = 0;
	      }
	  }
	
	dp[i][j]=v;
	tb[i][j]=d;
      }
  }
}

void dump(int len1,int len2)
{
  int i,j;
  for(i=0;i<=len1;i++)
    {
      for(j=0;j<=len2;j++)
	printf("%2d ",dp[i][j]);
      putchar('\n');
    }
}

int main()
{ int ret,r,i,len1,len2,len1a,len2a;

  i=-1;
    while(1)
    {
      r=scanf("%s",buf);
      if(r==EOF)
	break;
      else if('>'==buf[0])
	i++;
      else
	strcat(&dna[i][0],buf);
    }
  
    //test01();
    len1=strlen(&dna[0][0]);
    len2=strlen(&dna[1][0]);
    printf("len1=%d len2=%d\n",len1,len2);
    
    init_dp(len1,len2);
    fill_dp(len1,len2);

    //dump(len1,len2);

    /*
    fukugen(len1,len2);
    len1a=strlen(&aligned[0][0]);
    len2a=strlen(&aligned[1][0]);
    for(i=len1a-1;i>=0;i--)
      putchar(aligned[0][i]);
    putchar('\n');
    for(i=len2a-1;i>=0;i--)
      putchar(aligned[1][i]);
    */
    
    result=0;
    count_gap(len1,len2);
    printf("first:%d\n",result);
    ret=0;
    for(i=0;i<len1;i++)
      ret += ((aligned[0][i]=='-')?1:0);
    for(i=0;i<len2;i++)
      ret += ((aligned[1][i]=='-')?1:0);
    printf("confirm:%d\n",result);
  return(0);
}
