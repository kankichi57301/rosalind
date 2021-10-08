/*
rosalind
Complementing a Strand of DNA
*/

#include <stdio.h>
#include <string.h>

char dna[1001];
int main()
{
  
  int  i,len;
  
  scanf("%s",dna);

  len=strlen(dna);
  for(i=len-1;i>=0;i--)
    switch (dna[i])
      {
	case 'A':
	  putchar('T');
	  break;
	case 'C':
	  putchar('G');
	  break;
	case 'G':
	  putchar('C');	  
	  break;
	case 'T':
	  putchar('A');
	  break;
     }
  putchar('\n');
  return(0);
}
