#include <threads.h>
#include <stdbool.h>
#include <time.h>
#include <stdio.h>

typedef struct App {
        mtx_t m_mutex;
        bool m_bDataLoaded;
        cnd_t m_condVar;
} App_t;

int loadData(void *app)
{
        App_t *papp = app;
        struct timespec dura = {.tv_sec = 1};
        thrd_sleep(&dura, NULL);
        printf("Loading Data from XML\n");

        mtx_lock(&(papp->m_mutex));
        papp->m_bDataLoaded = true;
        cnd_signal(&(papp->m_condVar));
        mtx_unlock(&(papp->m_mutex));
}

int MainTask(void *app)
{
        printf("Do Some Handshaking\n");

        struct timespec dura = {.tv_nsec = 100000000};
        App_t *papp = app;
        mtx_lock(&(papp->m_mutex));
        cnd_wait(&(papp->m_condVar), &(papp->m_mutex));

        mtx_unlock(&(papp->m_mutex));

        printf("Do Processing On Loaded Data\n");
}

int main(int argc, char *argv[])
{
        App_t app = { .m_bDataLoaded = false};
        mtx_init(&(app.m_mutex), mtx_plain);
        cnd_init(&(app.m_condVar));

        thrd_t thread_1, thread_2;
        thrd_create(&thread_1, &MainTask, &app);
        thrd_create(&thread_2, &loadData, &app);

        thrd_join(thread_1, NULL);
        thrd_join(thread_2, NULL);

        return 0;
}
