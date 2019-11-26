// -nostdlib -lc
int printf(const char*, ...);
void exit(int);

void _start()
{
        printf("_start address: %zu\n", &_start);
        exit(0);
}
