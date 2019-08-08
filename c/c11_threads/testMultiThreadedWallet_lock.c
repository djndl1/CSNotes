#include <threads.h>
#include <stdio.h>

struct wallet {
        int mMoney;
        mtx_t mutex;
};
typedef struct wallet Wallet_t;

void Wallet_addMoney(Wallet_t* self, int money)
{
        mtx_lock(&(self->mutex));
        for (int i = 0; i < money; ++i)
                self->mMoney++;
mtx_unlock(&(self->mutex));
}

int addMoney_threadWrapper(void *args)
{
        Wallet_t* pwallet = args;
        Wallet_addMoney(pwallet, 1000);
}

// create 5 threads and each adds 1000 to the wallet
int testMultithreadedWallet()
{
        Wallet_t walletObj = { .mMoney = 0 };
        thrd_t threads[5];

        mtx_init(&(walletObj.mutex), mtx_plain);
        for (int i = 0; i < 5; ++i) {
                thrd_create(&threads[i], &addMoney_threadWrapper, &walletObj);
        }

        for (int i = 0; i < 5; ++i) {
                thrd_join(threads[i], NULL);
        }

        return walletObj.mMoney;
}

int main(int argc, char *argv[])
{
        int val = 0;
        for (int k = 0; k < 1000; k++) {
                if ((val = testMultithreadedWallet()) != 5000)
                        printf("Error at count = %d Money in Wallet = %d\n", k, val);
                
        }
        return 0;
}
