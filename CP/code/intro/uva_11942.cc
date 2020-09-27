#include <cstdio>

using namespace std;

int main(int argc, char* argv[]) 
{
    int N;
    scanf("%d", &N);
    
    printf("Lumberjacks:\n");
    while (N--) {
        bool ascend = true;
        bool descend = true;

        int prev;
        scanf("%d", &prev);
        for (int i = 1; i < 10; i++) {
            int cur;
            scanf("%d", &cur);

            if (cur > prev)
                descend = false;
            else if (cur < prev)
                ascend = false;
            
            prev = cur;
        }

        printf(descend || ascend ? "Ordered\n" : "Unordered\n");
    }

    
    return 0;
}