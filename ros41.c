/*
rosalind

 */

#include <stdio.h>
#include <string.h>

int n;

char name[10][81];
char buf[1001];
char buf2[1001];

long long memo[50];



int main()
{ int maxi=0,ret;
  int cnt=-1;
  double cg_ratio,maxratio=0.0;
  
  while(1)
    {
      ret=scanf("%s",buf);
      //printf("%d\n",cnt);
      if(buf[0]=='>'||ret==EOF)
	{
	  if(cnt>=0)
	    {
	      cg_ratio=calc_cg_ratio(buf2);
	      //printf("cg=%lf\n",cg_ratio);
	      if(cg_ratio > maxratio)
		{
		  maxi=cnt;
		  maxratio=cg_ratio;
		}
	    }
	   memset(buf2,0,sizeof(buf));
	  cnt++;
	  //strcpy(&name[cnt][0],&buf[1]);
	}
      else
	strcat(buf2,buf);
      if(ret==EOF)
	break;
     }
  printf("%s\n",&name[maxi][0]);
  printf("%lf\n",maxratio*100);
  return(0);
}
