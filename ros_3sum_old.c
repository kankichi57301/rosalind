/*
filename:ros_3sum.c
 */
#include <stdio.h>
int sum2(int ar[],int sum,int *pos1,int *pos2)
{
  printf("pos=%d,%d\n",*pos1,*pos2);
  if(*pos1>=*pos2)
    return(0);
  if(sum + ar[*pos1] + ar[*pos2] == 0 )
    return(1);
  if(sum +  ar[*pos1] + ar[*pos2] > 0)
    (*pos2)--;
  else
    (*pos1)++;
  return sum2(ar,sum,pos1,pos2);
}

int ta[]={1,2,4,6,7,12,16,19};
int main()
{
  int pos1,pos2,ret;
  pos1=0;pos2=7;
  ret=sum2(ta,-22,&pos1,&pos2);
  printf("ans=%d,%d:state=[%d]\n",pos1,pos2,ret);
  return(0);
}
