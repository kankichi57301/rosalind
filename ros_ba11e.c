/*
rosalind ba11e 
Sequence a Peptide
2021/08/07 AC
 */
#include <stdio.h>
#include <string.h>

struct prot
{ char amino;
  int  weight;
}
  protein_table[]={
    {'A',71},
    {'C',103},
    {'D',115},
    {'E',129},
    {'F',147},
    {'G',57},
    {'H',137},
    {'I',113},
    {'K',128},
    {'L',113},
    {'M',131},
    {'N',114},
    {'P',97},
    {'Q',128},
    {'R',156},
    {'S',87},
    {'T',101},
    {'V',99},
    {'W',186},
    {'Y',163}
    // {'X',4},
    // {'Z',5}
  };

int amino_weight[]={57 ,71 ,87 ,97 ,99 ,101 ,103 ,113 ,114 ,115 ,128 ,129 ,131 ,137 ,147 ,156 ,163 ,186};
int ws=18;

// spect vector
int S[20000];
int dp[20000];
int from[20000];
int res[20000];
int alllen;

extern void solve();
extern int backtrace();
extern char weight2amino(int);
int main()
{
  int i,ret,cnt;
  i=0;
  while(1)
  {
    ret=scanf("%d",&S[i]);
    if(ret==EOF)
      break;
    i++;
  }
  alllen=i;

  memset(dp,-1,sizeof(dp));
  dp[0]=0;
  solve();
  putchar('\n');
  /*
  for(i=1;i<=alllen;i++)
    printf("%d:",from[i]);
  putchar('\n');
  */
  
  cnt=backtrace();
  for(i=cnt;i>=0;i--)
    printf("%c",weight2amino(res[i]));
  putchar('\n');
  
  return(0);
}

void solve()
{
  int i,j,wt,pr,max_,max_a;
  for(i=1;i<=alllen;i++)
  {
    max_=0;
    for(j=0;j<ws;j++)
    {
      wt=amino_weight[j];
      if(wt>i)
	break;
      
      pr=dp[i-wt];
      if(pr<0)
      	continue;
      pr += S[i-1];
      
      if(pr > max_)
	{
	  max_=pr;
	  max_a = wt;
	}
    }
    if(max_>0)
      {
	dp[i]=max_;
	from[i]=max_a;
      }

  }
}

int backtrace()
{
  int i,p,next;
  p=alllen;
  i=0;
  do {
    next=from[p];
    //printf("%d;[%d]\n",p,next);
    res[i]=next;
    i++;
    p -= next;
  }  while(p>0);
  return(i-1);
}

char weight2amino(int w)
{
  int i,n = sizeof(protein_table)/sizeof(struct prot);
  for(i=0;i<n;i++)
    if(w == protein_table[i].weight)
      return(protein_table[i].amino);
  return('.');  // never reach
}
