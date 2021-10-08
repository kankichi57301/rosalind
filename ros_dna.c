/*
rosalind
Counting DNA Nucleotides
 */

#include <stdio.h>
#include <string.h>

char dna[1001];
int main()
{

  int  enki[4],i;
  
  scanf("%s",dna);
  memset(enki,0,sizeof(enki));
  for(char *p=dna;*p;p++)
    switch (*p)
      {
	case 'A':
	  enki[0]++;
	  break;
	case 'C':
	  enki[1]++;
	  break;
	case 'G':
	  enki[2]++;
	  break;
	case 'T':
	  enki[3]++;
	  break;
     }
  for(i=0;i<4;i++)
    {
      printf("%d",enki[i]);
      if(i<3)
	putchar(' ');
    }
  return(0);
}
