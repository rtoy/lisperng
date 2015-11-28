extern double WELLRNG512a(void);

int main()
{
  int k;
  int state[16];
  double sum;
  

  for (k = 0; k < 16; ++k) {
    state[k] = k;
  }

  InitWELLRNG512a(state);
  
  sum = 0.0;
  
  for (k = 0; k < 1000000; ++k) {
    sum += WELLRNG512a();
  }

  printf("sum = %16f\n", sum);

  for (k = 0; k < 10; ++k) {
    printf("%16.16f\n", WELLRNG512a());
  }
  
}
