#include "well44497a.h"

int main()
{
  int k;
  int state[1391];
  double sum;
  

  for (k = 0; k < 1391; ++k) {
    state[k] = k;
  }

  InitWELLRNG44497a(state);
  
  sum = 0.0;
  
  for (k = 0; k < 1000000; ++k) {
    sum += WELLRNG44497a();
  }

  printf("sum = %16f\n", sum);

  for (k = 0; k < 10; ++k) {
    printf("%16.16f\n", WELLRNG44497a());
  }
  
}
