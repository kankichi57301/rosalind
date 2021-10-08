/*
rosalind
Finding a Shared Motif
[LCSM]
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char dna[100][1001];
char buf[1001];
char name[100][81];

int cnt;
int len;
char enkistr[]="ACGT";
char *queue[100000];

int qptr,qptr2;
int maxlen;
char maxres[1001];

int k=3;

void pushq(char * s)
{
  queue[qptr]=s;
  qptr++;
}

void popq(char ** s)
{
  *s   = queue[qptr2];
  qptr2++;
}
  
int check(char *s)
{
  for(int i=0;i<cnt;i++)
    if(NULL==strstr(&dna[i][0],s))
      return 0;
    return(1);
}
void solve2()
{ char *p,*q;
  int  len,i;
  while(1) 
    {
      if(qptr2 >= qptr)
	return;
      popq(&p);
      //printf("pop=%s\n",p);
      len=strlen(p);

      if(!check(p))
	goto ENDLOOP;
      
      {
	  for(i=0;i<4;i++)
	  {
	    q = (char *)malloc(len+2);
	    strcpy(q,p);
	    q[len]=enkistr[i];
	    q[len+1]='\0';
	    pushq(q);
	  }
      }
      if(len>maxlen)
	{
	  strcpy(maxres,p);
	  maxlen=len;
	}
      
    ENDLOOP:
      if(len>1)
	free(p);
    }
}

char firststr[4][2];
void solve()
{
  maxlen=0;
  for(int i=0;i<4;i++)
  {
    firststr[i][0]=enkistr[i];
    pushq(&firststr[i][0]);
  }
  solve2();
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
  
  solve();

  printf("%s\n",maxres);
  
  return(0);
}
