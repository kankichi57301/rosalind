/*
rosalind
Identifying Unknown DNA Quickly
 */

#include <stdio.h>
#include <string.h>

int n;

char name[10][81];
char buf[1001];
char buf2[100001];
int  res[100001];

long long memo[50];

void solve()
{
  char *p,*q,*r,*s;
  int i;
  p=&(buf2[1]),q=&buf2[0];
  while(1)
    {
      r=strchr(p,*q);
      
      s=r;
      if(r==NULL)
	break;
      //else
	//printf("%d[%c][%c]\n",r-p,*r,*q);
      for(i=1;*r==*q ;i++,r++,q++)
      {
	//printf("%d[%c][%c]\n",r-buf2,*r,*q);
	
	if(res[r-buf2]==0)
	  res[r-buf2]=i;
      }
      p=s+1,q=buf2;
    }
}




int main()
{ 
  int siz,ret,i;

  memset(buf2,0,sizeof(buf2));
  memset(res ,0,sizeof(res ));  
  while(1)
    {
      ret=scanf("%s",buf);
      if(ret==EOF)
	break;
      else if(buf[0]!='>')  
	strcat(buf2,buf);
     }
  siz=strlen(buf2);
  
  solve();
  for(i=0;i<siz;i++)
    printf("%d ",res[i]);
    
  
  return(0);
}
