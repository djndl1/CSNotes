#include <cstdio>
#include <cstring>

char names[10][13];
int balance[10];

int find_name(const char *name, int n)
{
            int name_ind = -1;
            for (name_ind = 0; name_ind < n; name_ind++)
                if (strcmp(name, names[name_ind]) == 0)
                    break;

            return name_ind;
}

int main()
{
    int cases = 0;
    while (true) {
        cases++;
        int n;
        if ((scanf("%d\n", &n)) == EOF)
            break;
        for (int i = 0; i < n; i++) {
            scanf("%s", names[i]);
            balance[i] = 0;
        }
        getchar();

        for (int i = 0; i < n; i++) {
            char name[13];
            scanf("%s", name);
            int budget;
            scanf("%d", &budget);
            int persons;
            scanf("%d", &persons);
            if (persons == 0)
                continue;
            int avr_given = budget / persons;

            balance[find_name(name, n)] += - avr_given * persons;
            for (int j = 0; j < persons; j++) {
                char person_given[13];
                scanf("%s", person_given);
                balance[find_name(person_given, n)] += avr_given;
            }
            getchar();
        }

        if (cases > 1)
            putchar('\n');
        for (int i = 0; i < n; i++) {
            printf("%s %d\n", names[i], balance[i]);
        }
    }
}
