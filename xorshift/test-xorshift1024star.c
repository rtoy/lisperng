#include <stdio.h>
#include <stdint.h>

extern uint64_t xorshift64star(void);
extern uint64_t xorshift1024star(void);

int main()
{
    int k;
    uint64_t s[16];

    initxorshift64star(0x1234567887654321);

    printf("Seed:\n");
    for (k = 0; k < 16; ++k) {
	s[k] = xorshift64star();
	printf("%2d %zu\n", k, s[k]);
    }

    initxorshift1024star(s);

    for (k = 0; k < 16; ++k) {
	printf("%2d %zu\n", k, xorshift1024star());
    }

    return 0;
}
    
