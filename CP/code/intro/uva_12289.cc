#include <cstring>
#include <cstdio>

int main(int argc, char *argv[])
{
    int n;
    scanf("%d", &n);

    char str[6];
    while (n-- > 0) {
      scanf("%s", str);

      if (strlen(str) == 5)
        printf("3\n");
      else if (str[0] == 'o') {
        if (str[1] == 'w' && str[2] == 'o')
          printf("2\n");
        else
          printf("1\n");
      } else if (str[0] == 't') {
        if (str[1] == 'n' && str[2] == 'e')
          printf("1\n");
        else
          printf("2\n");
      } else {
        if (str[1] == 'w')
            printf("2\n");
        else
            printf("1\n");
      }
}
    return 0;
}
