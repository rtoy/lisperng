#include <stdio.h>

extern double WELLRNG1024a(void);
extern void InitWELLRNG1024a(unsigned int*);

int main()
{
  int k;
  unsigned int state[32];
  double sum;
  

  for (k = 0; k < 32; ++k) {
    state[k] = k;
  }

  InitWELLRNG1024a(state);
  
  sum = 0.0;
  
  for (k = 0; k < 1000000; ++k) {
    sum += WELLRNG1024a();
  }

  printf("sum = %.16g\n", sum);

  for (k = 0; k < 10; ++k) {
    printf("%16.16f\n", WELLRNG1024a());
  }
  
}
