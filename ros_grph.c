/*
rosalind
Overlap Graphs
[GRPH]
 */

#include <stdio.h>
#include <string.h>

char dna[100][10001];
char buf[1001];
char name[10][81];

int cnt;
int len;
char enkistr[]="ACGT";
int k=3;

int overlapped(char *s1,char *s2,int len)
{
  return (0==strncmp(s2,s1+strlen(s1)-len,len));
}


int main()
{
  int i,j;
  memset(dna,0,sizeof(dna));
  cnt=-1;
  while(EOF!=scanf("%s",buf))
    { 
      if(buf[0]=='>')
	{
	  cnt++;
	  strcpy(&name[cnt][0],&buf[1]);
	}
      else
	strcat(&dna[cnt][0],buf);
    }
  cnt++;

  for(i=0;i<cnt;i++)
    for(j=0; j<cnt;j++)
      {
	if(i==j)
	  continue;
	if(overlapped(&dna[i][0],&dna[j][0],k))
	  printf("%s %s\n",&name[i][0],&name[j][0]);
      }
  return(0);
}
