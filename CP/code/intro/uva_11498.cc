#include <cstdio>

using namespace std;

int main(int argc, char *argv[])
{
    int K;

    while (true) {
        scanf("%d", &K);
        if (K == 0)
            return 0;

        int N, M;
        int resX, resY;
        scanf("%d %d", &N, &M);
        while (K-- > 0) {
            scanf("%d %d", &resX, &resY);

            if (resX > N && resY > M) {
                printf("NE\n");
            } else if (resX > N && resY < M) {
                printf("SE\n");
            } else if (resX < N && resY > M) {
                printf("NO\n");
            } else if (resX < N && resY < M) {
                printf("SO\n");
            } else
                printf("divisa\n");
        }
    }
    return 0;
}
