#include <threads.h>
#include <stdio.h>

int WorkerThread(void *args)
{
    int *id = (int *) args;
    printf("WorkerThread %d is Executing\n", *id);
}

int main(int argc, char *argv[])
{
    thrd_t threadList[10];
    int threadID[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}; // It's doable but there is no actual portable way to print tid

    for (int i = 0; i < 10; i++) {
        thrd_create(&threadList[i], &WorkerThread, &threadID[i]);
    }

    printf("Waiting for all the worker threads to finish\n");

    for (int i = 0; i < 10; i++) {
        thrd_join(threadList[i], NULL);
    }

    printf("Exiting from Main Thread\n");
    return 0;
}
