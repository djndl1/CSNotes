#include <threads.h>
#include <stdbool.h>
#include <time.h>
#include <stdio.h>

typedef struct App {
        bool m_bDataLoaded;
} App_t;

int loadData(void *app)
{
        App_t *papp = app;
        struct timespec dura = {.tv_sec = 1};
        thrd_sleep(&dura, NULL);
        printf("Loading Data from XML\n");

        papp->m_bDataLoaded = true;
}

int MainTask(void *app)
{
        printf("Do Some Handshaking\n");

        struct timespec dura = {.tv_nsec = 100000000};
        App_t *papp = app;
        while ((papp->m_bDataLoaded) != true) {
                thrd_sleep(&dura, NULL);

        }


        printf("Do Processing On Loaded Data\n");
}

int main(int argc, char *argv[])
{
        App_t app = { .m_bDataLoaded = false};

        thrd_t thread_1, thread_2;
        thrd_create(&thread_1, &MainTask, &app);
        thrd_create(&thread_2, &loadData, &app);

        thrd_join(thread_1, NULL);
        thrd_join(thread_2, NULL);

        return 0;
}
