#include <threads.h>
#include <stdio.h>

struct intStr {
        int x;
        char *str;
};

int threadCallBack(void* args)
{
        struct intStr* passingArgs = args;
        printf("Passed Number = %d\n", passingArgs->x);
        printf("Passed String = %s\n", passingArgs->str);
}

int main(int argc, char *argv[])
{
        struct intStr args;

        args.x = 10;
        args.str = "Sample String";

        thrd_t threadObj;
        thrd_create(&threadObj, &threadCallBack, &args);
        thrd_join(threadObj, NULL);
        return 0;
}
