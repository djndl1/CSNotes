#include <threads.h>
#include <stdio.h>

int thread_function(void* id)
{
    int* thread_ID = (int *) id;
    printf("Inside Thread::ID = %u\n", *thread_ID);
}

int main(int argc, char *argv[])
{
    thrd_t threadObj1, threadObj2;
    size_t threadID1 = 1, threadID2 = 2;
    thrd_create(&threadObj1, &thread_function, &threadID1);
    thrd_create(&threadObj2, &thread_function, &threadID2);

    if (thrd_equal(threadObj1, threadObj2) == 0)
        printf("From Main Thread\n" "Two threads are not the same\n");
    
    thrd_join(threadObj1, NULL);
    thrd_join(threadObj2, NULL);

    return 0;
}

