#include <stdint.h>

static uint64_t s[16];
static int p;

uint64_t xorshift1024star(void)
{
    uint64_t s0;
    uint64_t s1;
    s0 = s[p];
    p = (p + 1) %15;
    s1 = s[p];
    s1 ^= s1 << 31;
    s1 ^= s1 >> 11;
    s0 ^= s0 >> 30;
    s[p] = s0 ^ s1;
    return s[p] * UINT64_C(1181783497276652981);
}

void initxorshift1024star(uint64_t* init)
{
    int k;

    for (k = 0; k < 16; ++k) {
	s[k] = init[k];
    }

    p = 0;
}
