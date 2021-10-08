/*
rosalind
Inferring mRNA from Protein
[MRNA]
 */

#include <stdio.h>
#include <string.h>

#define HOU 1000000

char protain[1001];
int amino_vari[27];

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
  '@','Q','K','E',
  '@','Q','K','E',
  'C','R','S','G',
  'C','R','S','G',
  '@','R','R','G',
  'W','R','R','G' };



void make_amino_vari()
{
  int i,j,k;
  memset(amino_vari,0,sizeof(amino_vari));
  for(i=0;i<4;i++)
    for(j=0;j<4;j++)
      for(k=0;k<4;k++)
	amino_vari[table[i][j][k]-'@']++;
}

int main()
{
  char *p;
  int ret=1;
  make_amino_vari();
  scanf("%s",protain);
  for(char *p=protain;*p;p++)
    ret = (ret * amino_vari[*p-'@'])%HOU;
  ret = (ret * 3)%HOU; // stop kodon
  printf("%d\n",ret);

  return(0);
}
