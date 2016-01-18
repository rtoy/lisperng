#include <stdio.h>
#include <stdint.h>

extern void initxorshift64star(uint64_t);
extern uint64_t xorshift64star(void);
extern void initxorshift1024star(uint64_t* s);
extern uint64_t xorshift1024star(void);

int main()
{
    int k;
    uint64_t s[16];

    initxorshift64star(0x1234567887654321);

    printf("Seed:\n");
    for (k = 0; k < 16; ++k) {
	s[k] = xorshift64star();
	printf("%2d %llu\n", k, s[k]);
    }


    initxorshift1024star(s);

    printf("Outputs:\n");
    for (k = 0; k < 32; ++k) {
	printf("%2d %llu\n", k, xorshift1024star());
    }

    return 0;
}
    
