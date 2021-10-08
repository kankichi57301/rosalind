/*
Rosalind
Building a Heap  
[HEA] 2012/05/12 AC
 */
#include <stdio.h>
int n;
int a[100001];

void build_max_heap(int i)
{
  int l,r,largest,w;
  
  l= 2*i+1;
  r= 2*i+2;
  if(l<n && a[l]>a[i])
  {
    //putchar('@');
    largest=l;
  }
  else
    largest=i;
  if(r<n && a[r]>a[largest])
  {
    //putchar('*');
    largest=r;
  }
  //printf("%d[%d] %d[%d] %d[%d]=>%d[%d]\n",i,a[i],l,a[l],r,a[r],largest,a[largest]);
  if(largest != i)
    {
      w=a[largest],a[largest]=a[i],a[i]=w;
      build_max_heap(largest);
    }
}

void solve()
{
  int i;
  for(i=n/2-1;i>=0;i--)
    build_max_heap(i);
}

int main()
{
  int i;
  scanf("%d",&n);
  for(i=0;i<n;i++)
    scanf("%d",&a[i]);

  solve();
  for(i=0;i<n;i++)
    printf("%d ",a[i]);
  putchar('\n');
  return(0);
}
  
  
