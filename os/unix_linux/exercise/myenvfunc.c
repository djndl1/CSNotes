#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <stdbool.h>

extern char **environ;

int myunsetenv(const char *name)
{
        if (name == NULL || strlen(name) == 0 || strchr(name, '=')) {
                errno = EINVAL;
                return -1;
        }

        unsigned long cur_pos = 0;
        bool has_name = false;
        while (environ[cur_pos]) {
                if (strstr(environ[cur_pos], name)) {
                        has_name = true;
                        break;
                }
                cur_pos++;
        }
        if (!has_name)
                return 0;

        while (environ[cur_pos]) {
                environ[cur_pos] = environ[cur_pos + 1];
                cur_pos++;
        }

        return 0;
}

int mysetenv(const char *name, const char *value, int overwrite)
{
        if (name == NULL || strlen(name) == 0 || strchr(name, '=')) {
                errno = EINVAL;
                return -1;
        }

        const unsigned long env_len = strlen(name) + strlen(value) + 2;
        char *nv = malloc(env_len);
        if (nv == NULL) {
                errno = ENOMEM;
                return -1;
        }

        if (snprintf(nv, env_len,"%s=%s", name, value) != env_len - 1) 
                goto cleanup;

        if (getenv(name)) {
                if (overwrite) {
                        if (myunsetenv(name))
                                goto cleanup;
                } else {
                        return 0;
                }
        }
        if (putenv(nv))
                goto cleanup;

        return 0;
cleanup:
        free(nv);
        return -1;
}

int main(int argc, char *argv[])
{
        char **ep;

        clearenv();

        if (mysetenv("abc", "123", 0)) {
                perror("You failed to add abc to env");
                return -1;
        }

        printf("abc=%s\n", getenv("abc"));
        if (mysetenv("abc", "456", 1)) {
                perror("You failed to add abc to env");
                return -1;
        }
        printf("abc=%s\n", getenv("abc"));
        myunsetenv("abc");
        if (!getenv("abc"))
                printf("abc is not an environment variable\n");

        return 0;
}
