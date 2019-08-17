#include <stdio.h>
#include <stdbool.h>
#include <threads.h>
#include <stdint.h>
#include <time.h>

typedef struct semaphore {
        volatile int    d_available;
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
        sm->d_available--;
        while (sm->d_available < 0)
                cnd_wait(&sm->d_cnd, &sm->d_mtx);
        printf("Semaphore %zu count %d\n", sm->id, sm->d_available);
        mtx_unlock(&sm->d_mtx);
}

void smph_notify_all(smph_t *sm)
{
        mtx_lock(&sm->d_mtx);
        while (sm->d_available < 0) {
                sm->d_available++;
                if (sm->d_available == 0)
                        cnd_broadcast(&sm->d_cnd);
        }
        printf("Semaphore %zu count %d\n", sm->id, sm->d_available);
        mtx_unlock(&sm->d_mtx);
}




typedef struct sems {
        smph_t empty_count;
        smph_t full_count;
        mtx_t pool_mtx;
        size_t items;
} sems_t;

struct timespec consuming_dura = { 1, 0 };
struct timespec producing_dura = { 2, 0 };

int consumer(void *arg)
{
        sems_t *sms = arg;
        while (true) {
                smph_wait(&sms->full_count);  // P
                mtx_lock(&sms->pool_mtx);
                printf("Consuming, %zu remaining in the pool\n", sms->items);
                thrd_sleep(&consuming_dura, NULL);
                mtx_unlock(&sms->pool_mtx);
                smph_notify_all(&sms->empty_count); // V
        }
}

int producer(void *arg)
{
        sems_t *sms = arg;

        while (true) {
                smph_wait(&sms->empty_count);   // P
                mtx_lock(&sms->pool_mtx);
                sms->items++;
                printf("Producing, %zu now in the pool\n", sms->items);
                thrd_sleep(&producing_dura, NULL);
                mtx_unlock(&sms->pool_mtx);
                smph_notify_all(&sms->full_count);  // V
        }
}


int main(int argc, char *argv[])
{
        smph_t empty, full;
        smph_init(&full, 0, 1);
        smph_init(&empty, 5, 2);
        mtx_t pool_t;
        mtx_init(&pool_t, mtx_plain);
        sems_t sms = { empty, full, pool_t, 0};

        thrd_t consume_thrd1, consume_thrd2, produce_thrd;
        thrd_create(&consume_thrd2, consumer, &sms);
        thrd_create(&consume_thrd1, consumer, &sms);
        thrd_create(&produce_thrd, producer, &sms);

        thrd_join(consume_thrd1, NULL);
        thrd_join(consume_thrd2, NULL);
        thrd_join(produce_thrd, NULL);

        return 0;
}
