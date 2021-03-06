#include <stdio.h>

extern double WELLRNG19937a(void);
extern void InitWELLRNG19937a(unsigned int*);

int main()
{
  int k;
  unsigned int state[624];
  double sum;
  

  for (k = 0; k < 624; ++k) {
    state[k] = k;
  }

  InitWELLRNG19937a(state);
  
  sum = 0.0;
  
  for (k = 0; k < 1000000; ++k) {
    sum += WELLRNG19937a();
  }

  printf("sum = %16f\n", sum);

  for (k = 0; k < 10; ++k) {
    printf("%16.16f\n", WELLRNG19937a());
  }
  
}
