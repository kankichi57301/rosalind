/*
rosalind
Transcribing DNA into RNA
*/

#include <stdio.h>
#include <string.h>
char dna[1001];
int main()
{
  scanf("%s",dna);
  for(char *p=dna;*p;p++)
    putchar((*p)=='T'?'U':(*p));
	    
  return(0);
}
