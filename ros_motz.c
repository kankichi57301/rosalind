/*
Rosalind
Motzkin Numbers and RNA Secondary Structures
[MOTZ]
 */
#include <stdio.h>
#include <string.h>

char name[100];
char rna[301];
char buf[100];

//int enki_c[300][300][4];
int memo[300][300];
int len;

int enki2n(char c)
{
  switch (c)
    {
    case 'A':
      return 0;
    case 'C':
      return 1;
    case 'G':
      return 2;
    case 'U':
      return 3;
    }
  return(-1);
}

//
//node-a‚©‚ç node-b‚ÌŠÔ‚Ìnode‚Ç‚¤‚µ‚ð‚Â‚È‚®•û–@‚Ì”
//flag=0 a‚Æb‚ð‚Â‚È‚®‚±‚Æ‚ð‹­§
//flag=1 a‚Íb‚æ‚è‹ß‚¢node‚É‚Â‚È‚°‚Ä‚à‚æ‚¢
//
int solve(int a,int b)
{
  int ret,ret2;
  int i,c;
  long long m,m1,m2;

  //printf("arg=%d %d\n", a, b);

  
  if((ret=memo[a][b])>=0)
    goto END;
  

  ret=solve(a+1,b-1);

  for(i=a+1;i<=b-2;i++)
      {
	if(rna[a]+rna[i]==3)
	{
	    m1 = solve(a,i);
	    m2 = solve(i+1,b);
	    m = m1*m2; 
	    m %= 1000000;
	    ret = (ret + m) % 1000000 ;
	}
      }

  memo[a][b]=ret;

 END:
  //if(ret<0)
  printf("arg=%d %d[%d]\n", a, b,ret);
  return(ret);
  
}
void prepare()
{
  int i,j,k;
  for(i=0;i<len;i++)
    rna[i]=enki2n(rna[i]);



 
  for(i=0;i<len;i++)
    {
      memo[i][i]=1;
    if(rna[i]+rna[i+1]==3)
      memo[i][i+1]=2;
    else
      memo[i][i+1]=1;
    }
}




int main()
{ int ret,r;

  memset(memo,-1,sizeof(memo));

  scanf("%s",name);
  while(1)
    {
      r=scanf("%s",buf);
      if(r==EOF)
	break;
      strcat(rna,buf);
    }
  len=strlen(rna);
  printf("name=%s rna=%s len=%d\n",name,rna,len);
  prepare();


  ret=solve(0,len-1);
  printf("%d\n",ret);

  return(0);
}
/*
	Motzkin numbers: number of ways of drawing any number of nonintersecting chords joining n (labeled) points on a circle.

	1, 1, 2, 4, 9, 21, 51, 127, 323, 835, 2188, 5798, 15511, 41835, 113634, 310572, 853467, 2356779, 6536382, 18199284, 50852019, 142547559, 400763223, 1129760415, 3192727797, 9043402501, 25669818476, 73007772802, 208023278209, 593742784829
 */
