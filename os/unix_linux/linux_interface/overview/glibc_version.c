#include <gnu/libc-version.h>

#include <stdio.h>

int main(int argc, char *argv[])
{
    const char *libc_version = gnu_get_libc_version();
    const char *libc_release = gnu_get_libc_release();

    printf("Platform glibc: %s-%s\n", libc_version, libc_release);
    printf("Target glibc: %d.%d", __GLIBC__, __GLIBC_MINOR__);
}
