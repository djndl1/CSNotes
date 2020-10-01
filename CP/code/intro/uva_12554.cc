#include <cstdio>

const char *lyrics[] = {
    "Happy", "birthday", "to", "you",
    "Happy", "birthday", "to", "you",
    "Happy", "birthday", "to", "Rujia",
    "Happy", "birthday", "to", "you",
};

int main(int argc, char *argv[])
{
    int n;
    scanf("%d", &n);

    char names[100][101];
    for (int i = 0; i < n; i++) {
        scanf("%s", names[i]);
    }

    int curWord = 0, peopleSing= 0;
    const int lyricLen = sizeof(lyrics) / sizeof(char*);
    while (peopleSing < n || curWord != lyricLen) {
        curWord %= lyricLen;

        printf("%s: %s\n", names[peopleSing % n], lyrics[curWord]);

        curWord++;
        peopleSing++;
    }

    return 0;
}
