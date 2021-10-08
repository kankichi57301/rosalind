/*
filename:ros_3sum.c
 */
#include <stdio.h>
#include <stdlib.h>
int m,n;
int sum2(int ar[],int siz,int sum,int *ans1,int *ans2)
{ int *pos1,*pos2;
  pos1=ar,pos2=ar+(siz-1);
  
  while(1)
  {
    if(*pos1>=*pos2)
      return(0);
    if(sum + *pos1 + *pos2 == 0 )
    {
      *ans1=*pos1,*ans2=*pos2;
      return(1);
    }
    if(sum + *pos1 + *pos2 > 0)
      pos2--;
    else
      pos1++;
  }
}

int sum2A(int ar[],int siz,int sum,int *ans1,int *ans2,int except)
{ int *pos1,*pos2;
  pos1=ar,pos2=ar+(siz-1);
  
  while(1)
  {
    if (pos1-ar==except)
    {
      pos1++;
      continue;
    }
    if (pos2-ar==except)
    {
      pos2--;
      continue;
    }
    
    if(*pos1>=*pos2)
      return(0);
    if(sum + *pos1 + *pos2 == 0 )
    {
      *ans1=*pos1,*ans2=*pos2;
      return(1);
    }
    if(sum + *pos1 + *pos2 > 0)
      pos2--;
    else
      pos1++;
  }
}
int sum3(int ar[],int siz,int *v1 ,int *v2,int *v3)
{
  int i,ret,u2,u3;
  for(i=0;ar[i]<=0 && i<siz;i++)
    {
      ret=sum2A(ar,siz,ar[i],&u2,&u3,i);
      if(ret)
	{
	  *v1=ar[i],*v2=u2,*v3=u3;
	  return(1);
	}
    }
  return(0);
} 


int ta[]={-62,-34,4,7,12,16,19,21,25,28,33,37,42};
int main()
{
  int ans1,ans2,ans3,siz,ret;
  siz=sizeof(ta)/sizeof(int);
  ret=sum3(ta,siz,&ans1,&ans2,&ans3);
  printf("ans=%d,%d,%d:state=[%d]\n",ans1,ans2,ans3,ret);
  return(0);
}

/*
int main()
{
  int ans1,ans2,siz,ret,wa;
  siz=sizeof(ta)/sizeof(int);wa=-49;
  ret=sum2(ta,siz,wa,&ans1,&ans2);
  printf("ans=%d,%d:state=[%d]\n",ans1,ans2,ret);
  return(0);
}
*/
#if 0
struct num_st
{ int val;
  int pos;
} numbers[100001];
int work[100001];
void prepare()
{


}



int main()
{
  int i,j,x,ret,v1,v2,v3;
  scanf("%d %d",&m,&n);
  for(i=0;i<m;i++)
  {
    for(j=0;j<n;j++)
    {
      scanf("%d",&x);
      numbers[j].val = x
      numbers[j].pos=i+1;
      work[j]=x;
    }
    //prepare();
    //ret=solve_3sum(work,n,&v1,&v2,&v3);
    /*
    if(ret)
           printf("1 %d %d %d\n",calc_index(v1),calc_index(v2),cal_index(v3));
    else
           printf("-1\n");
    */
  }
  return(0);
}

int comp_st( const void * a , const struct num_st * b ) {

  if( (( struct num_st *)a)->val > ((struct num_st * )b)->val ) {
    return 1;
  }
  else
  if( (( struct num_st *)a)->val == ((struct num_st * )b)->val ) {
    return 0;
  }
  return -1;
}

int comp_int( const void * a , const struct num_st * b ) {

  if( *((int *)a) > +((int * )b) ) {
    return 1;
  }
  else
  if( *((int *)a) == *((int * )b) )
    return 0;
  }
  return -1;
}
#endif
