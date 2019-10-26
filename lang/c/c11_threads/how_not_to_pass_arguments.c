#include <threads.h>
#include <stdio.h>
#include <time.h>


int newThreadCallback(void *args)
{
        int *ip = (int*) args;
        printf("Inside Thread: %d\n", *ip);
        struct timespec dura = {.tv_sec = 1.0};
        thrd_sleep(&dura, NULL);
        *ip = 19;
}

void startNewThread(void)
{
        int i = 10;
        printf("Inside Main Thread : i=%d\n", i);
        thrd_t t;
        thrd_create(&t, &newThreadCallback, &i);
        thrd_detach(t);
        printf("Inside Main Thread: i=%d\n", i);
}

int main(int argc, char *argv[])
{
        startNewThread();
        struct timespec dura = { .tv_sec = 2 };
        thrd_sleep(&dura, NULL);
        return 0;
}
