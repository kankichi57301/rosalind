/*
rosalind
Translating RNA into Protein 
[PROT]
 */

#include <stdio.h>
#include <string.h>

char dna[10001];
char table[4][4][4]={
  'F','L','I','V',
  'F','L','I','V',
  'L','L','I','V',
  'L','L','M','V',
  'S','P','T','A',
  'S','P','T','A',
  'S','P','T','A',
  'S','P','T','A',
  'Y','H','N','D',
  'Y','H','N','D',
  ' ','Q','K','E',
  ' ','Q','K','E',
  'C','R','S','G',
  'C','R','S','G',
  ' ','R','R','G',
  'W','R','R','G' };


int enki2n(char e)
{
  switch(e)
    {
    case 'U':
      return 0;
    case 'C':
      return 1;
    case 'A':
      return 2;
    case 'G':
      return 3;
    }
  return(-1); // error
}

char conv_trple(char *p)
{
  return table[enki2n(p[1])][enki2n(p[2])][enki2n(p[0])];
}

int main()
{
  char *p;
  
  scanf("%s",dna);
  for(char *p=dna;*p;p+=3)
    putchar(conv_trple(p));

  putchar('\n');

  return(0);
}
