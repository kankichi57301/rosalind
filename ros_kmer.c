/*
rosalind
k-Mer Composition
 */

#include <stdio.h>
#include <string.h>

int n;

char name[10][81];
char buf[1001];
char buf2[100001];
int mer4[256];


int enki2num(char c)
{
  switch(c)
    {
    case 'A':
      return(0);
    case 'C':
      return(1);
    case 'G':
      return(2);
    case 'T':
      return(3);
    }

}


int conv4mer2num(char *s)
{
  return (64*enki2num(s[0]) +\
	  16*enki2num(s[1]) +
	  4 *enki2num(s[2]) +
	     enki2num(s[3]));
}
void out4mer(int m4[])
{
  int i;
  for(i=0;i<255;i++)
    printf("%d ",m4[i]);
  printf("%d",m4[255]);
}

void solve(char *s,int m4[])
{
  int len = strlen(s);
  for(int i=0;i<=len-4;i++)
    m4[conv4mer2num(s+i)]++;
}



int main()
{ int i,ret;

  memset(buf2,0,sizeof(buf));  
  while(1)
    {
      ret=scanf("%s",buf);
      if(ret==EOF)
	break;
      else if(buf[0]!='>')  
	strcat(buf2,buf);
     }

  solve(buf2,mer4);
  out4mer(mer4);
  
  return(0);
}
