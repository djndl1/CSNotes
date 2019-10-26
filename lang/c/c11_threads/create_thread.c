#include <threads.h>
#include <stdio.h>

int thread_function(void *args)
{
    for (int i = 0; i < 10000; i++)
        printf("thread function Executing\n");

    return 0;
}

int main(int argc, char *argv[])
{ 
    thrd_t threadObj;
    int th_err = 0;
    th_err = thrd_create(&threadObj, &thread_function, NULL);

    if (th_err != thrd_success)
        goto thread_fail;

    for (int i = 0; i < 10000; i++)
        printf("Display from MainThread\n");
    thrd_join(threadObj, NULL);

    return 0;

thread_fail:
    fprintf(stderr, "Failed to create a thread\n");
}
