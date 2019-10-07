#include <cstdio>

int main(int argc, char *argv[])
{
    bool quote = false;
    int c;

    while ((c = getchar()) != EOF) {
        if (c == '"') {
            printf("%s",quote ? "''" : "``");
            quote = !quote;
        } else
            putchar(c);
    }

    return 0;
}
