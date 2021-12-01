#include "stdio.h"

#define min(a, b) a<b?a:b

int main(){
  int slides[3] = {0, 0, 0};
  int a, o, c = 0;
  int p = 0;
  int inc = 0;
  
  while(scanf("%d", &a) != EOF){
    int k = min(c+1, 3);
    for(int i=0;i<k;i++) {
      slides[(o+i)%3] += a;
    }

    if(c >= 2){
      int d = slides[o%3];

      if(!p)
        p = d;
      if(p < d)
        inc++;
      p=d;
      
      slides[o%3] = 0;
      o++;
    }

    c++;
  }
  printf("%d\n", inc);
}
