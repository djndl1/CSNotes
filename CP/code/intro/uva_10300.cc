#include <cstdio>

int main(int argc, char *argv[]) {
  int cases;
  scanf("%d", &cases);

  while (cases-- > 0) {
    int farmers;
    scanf("%d", &farmers);

    int budget = 0;
    while (farmers-- > 0) {
        int size, animals, env_frie;
        scanf("%d %d %d", &size, &animals, &env_frie);

        budget += size * env_frie;
    }
    printf("%d\n", budget);
  }

  return 0;
}
