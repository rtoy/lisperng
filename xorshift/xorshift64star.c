#include <stdint.h>

static uint64_t x;

uint64_t xorshift64star(void)
{
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    return x * UINT64_C(2685821657736338717);
}

void initxorshift64star(uint64_t seed)
{
    x = seed;
}
