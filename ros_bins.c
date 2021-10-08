/*
rosalind [BINS]
binary serach
 */
#include <stdio.h>

int m,n;
int A[100001],k[100001];

int bin_search(int ar[],int from,int to,int val)
{
  int mid,midv;
  if(ar[from]==val)
    return from;
  if(ar[to]==val)
    return to;
  if(to - from <2)  // not find
    return -2;
  mid=(from+to)/2;
  midv=ar[mid];
  if(midv==val)
    return mid;
  if(midv <val)
    return bin_search(ar,mid,to,val);
  else
    return bin_search(ar,from,mid,val);
}

int main()
{
  int i;
  scanf("%d",&n);
  scanf("%d",&m);
  for(i=0;i<n;i++)
    scanf("%d",&A[i]);
  for(i=0;i<m;i++)
    scanf("%d",&k[i]);

  for(i=0;i<m;i++)
    printf("%d ",1+bin_search(A,0,n-1,k[i]));
  putchar('\n');

  return 0;
}
