#include <cstdio>
#include <float.h>

int main()
{
    double height, up, down, fatigue;
    while (scanf("%lf %lf %lf %lf\n", &height, &up, &down, &fatigue), height) {
        int day = 1;
        double dist = up;
        double highest = 0;
        double afterSliding;

        const double fatigueOffset = up * fatigue / 100.0;
        bool succeeded;
        while (true) {
            highest += dist;
            if (highest > height) {
                succeeded = true;
                break;
            }

            highest = highest - down;
            if (highest < 0) {
                succeeded = false;
                break;
            }

            dist -= fatigueOffset;
            if (dist < 0)
                dist = 0;

            day++;

          //  printf("day %d: %lf %lf %lf %lf\n",
           //     day, initOfDay, climbed, afterClimbing, afterSliding);
        }
        if (succeeded) {
            printf("success on day %d\n", day);
        } else {
            printf("failure on day %d\n", day);
        }
    }
}
