#include <stdio.h>
#include <stdbool.h>
#include <threads.h>
#include <stdint.h>
#include <time.h>

typedef struct semaphore {
        size_t    d_available;
        cnd_t     d_cnd;
        mtx_t     d_mtx;

        size_t    id;
} smph_t;

int smph_init(smph_t *sm, size_t amount, size_t id)
{
        int err;

        if ((err = mtx_init(&sm->d_mtx, mtx_plain)) != thrd_success)
                return -1;
        if ((err = cnd_init(&sm->d_cnd)) != thrd_success)
                return -2;
        sm->d_available = amount;
        sm->id = id;

        return 0;
}

void smph_destroy(smph_t *sm)
{
        sm->d_available = 0;
        cnd_destroy(&sm->d_cnd);
        mtx_destroy(&sm->d_mtx);
}

void smph_wait(smph_t *sm)
{
        mtx_lock(&sm->d_mtx);
        while (sm->d_available == 0)
                cnd_wait(&sm->d_cnd, &sm->d_mtx);

        sm->d_available--;
        printf("Semaphore %lu resource consumed, remaining %lu\n", sm->id, sm->d_available);
        mtx_unlock(&sm->d_mtx);
}

void smph_notify_all(smph_t *sm)
{
        mtx_lock(&sm->d_mtx);
        while (sm->d_available++ == 0)
                cnd_broadcast(&sm->d_cnd);
        printf("Semaphore %lu resource increased, now %lu\n", sm->id, sm->d_available);
        mtx_unlock(&sm->d_mtx);
}




typedef struct sems {
        smph_t filled;
        smph_t available;
} sems_t;

struct timespec consuming_dura = { 5, 0 };
struct timespec producing_dura = { 4, 0 };

int consumer(void *arg)
{
        sems_t *sms = arg;
        while (true) {
                printf("Consuming\n");
                smph_wait(&sms->filled);  // waiting for resource available
                thrd_sleep(&consuming_dura, NULL);
                smph_notify_all(&sms->available); // notify producer if there is no resource
        }
}

int producer(void *arg)
{
        sems_t *sms = arg;

        while (true) {
                smph_wait(&sms->available);   // waiting for an empty place to place resource
                printf("Producing\n");
                thrd_sleep(&producing_dura, NULL);
                smph_notify_all(&sms->filled);  // notify consumers there is a resource available
        }
}

int main(int argc, char *argv[])
{
        smph_t filled, available;
        smph_init(&filled, 0, 1);
        smph_init(&available, 5, 2);
        sems_t sms = { filled, available };

        thrd_t consume_thrd1, consume_thrd2, produce_thrd;
        thrd_create(&consume_thrd2, consumer, &sms);
        thrd_create(&consume_thrd1, consumer, &sms);
        thrd_create(&produce_thrd, producer, &sms);

        thrd_join(consume_thrd1, NULL);
        thrd_join(consume_thrd2, NULL);
        thrd_join(produce_thrd, NULL);

        return 0;
}
