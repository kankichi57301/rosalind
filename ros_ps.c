/*
Rosalind
filename:ros_ps.c
2021/05/ AC
 */
int A[100001],n,k;

#include <stdio.h>
#include <stdlib.h>

int comp_int( const void * a , const void * b )
{
  if( *((int *)a) > *((int * )b) ) 
    return 1;
  if( *((int *)a) == *((int * )b) )
    return 0;
  return -1;
}



int main()
{
  int i;
  scanf("%d",&n);
  for(i=0;i<n;i++)
    scanf("%d",&A[i]);
  scanf("%d",&k);
  qsort(A,n,sizeof(int),comp_int);

  for(i=0;i<k;i++)
    printf("%d ",A[i]);
  
  return(0);
}

