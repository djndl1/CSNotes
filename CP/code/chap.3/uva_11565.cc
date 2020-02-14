#include <bits/stdc++.h>
using namespace std;

int main(int argc, char *argv[])
{
    int N;
    scanf("%d\n", &N);

    while (N--) {
        int A, B, C;
        bool hasSolution = false;
        scanf("%d %d %d\n", &A, &B, &C);
        
        for (int x = -100; x <= 100; x++)
            if (x*x <= C)
                for (int y = -100; y <= 100; y++)
                    if (x !=y && (x*x + y*y <= C))
                        for (int z = -100; z <= 100; z++) {
                            if (x != y && y != z && z != x && (x + y + z == A) &&
                    (x * y * z == B) && (x * x + y * y + z * z == C)) {
                    printf("%d %d %d\n", x, y, z);
                    hasSolution = true;
                    goto solved;
                }
              }
    solved:
        if (not hasSolution)
            printf("No solution.\n");
    }
    return 0;
}

