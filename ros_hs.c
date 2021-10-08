/*
Rosalind
heap sort
[HS]
 */
#include <stdio.h>

int ar[100001];
int heap[100001];
int res[100001];
int n;

void restr_heap(int heap[],int node)
{
  int par,w;
  if(node==0)
    return;
  par=(node-1)/2;
  if(heap[par]<heap[node])
  {
      w = heap[par];
      heap[par]=heap[node];
      heap[node]=w;
      restr_heap(heap,par);
  }
}


void build_heap(int ar[],int heap[],int n)
{
  int i;
  for(i=0;i<n;i++)
  {
    heap[i]=ar[i];
    restr_heap(heap,i);
  }
}
  
void rebuild_heap(int heap[],int node,int n)
{
  int w,c1,c2;

  c1=node*2+1,c2=node*2+2;
  //printf("node=%d %d c1=%d c2=%d\n",node,heap[node],heap[c1],heap[c2]);
  if(c1>=n)
    return;
  if(heap[c1]> heap[c2] && heap[c1]>heap[node])
  {
    w=heap[c1];heap[c1]=heap[node];heap[node]=w;
    rebuild_heap(heap,c1,n);
  }
  else if(heap[c2]> heap[c1] && heap[c2]>heap[node])
  {
    w=heap[c2];heap[c2]=heap[node];heap[node]=w;
    rebuild_heap(heap,c2,n);
  }
}


void h_sort(int ar[],int n)
{
  int *out;
  out=&res[n-1];
  build_heap(ar,heap,n);

  
  while(n>0)
  {
    *out-- = heap[0];
    n--;
    heap[0]=heap[n];
    rebuild_heap(heap,0,n);
  }
  
}


int main()
{
  int i;
  scanf("%d",&n);
  for(i=0;i<n;i++)
    scanf("%d",&ar[i]);
	      
  h_sort(ar,n);
 
  for(i=0;i<n;i++)
    printf("%d ",res[i]);
  putchar('\n');

  return(0);
}
