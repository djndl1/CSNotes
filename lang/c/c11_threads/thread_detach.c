#include <threads.h>
#include <stdio.h>

int thread_function(void *args)
{
    for (int i = 0; i < 10000; i++)
        printf("thread_function executing\n");
}

int main(int argc, char *argv[])
{
    thrd_t threadObj;

    thrd_create(&threadObj, &thread_function, NULL);

    if (thrd_detach(threadObj) == thrd_error)
      fprintf(stderr, "Failed to detach!\n");

    return 0;
}
