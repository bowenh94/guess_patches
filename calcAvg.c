#include "stdio.h"
#include "stdlib.h"

int main(){
  FILE *fp = fopen("output.txt","r");
  int n = 0;
  int sum = 0;
  while(fscanf(fp,"%d",&n)!=EOF){
    sum += n;
  }
  printf("sum is %d",sum);
}
