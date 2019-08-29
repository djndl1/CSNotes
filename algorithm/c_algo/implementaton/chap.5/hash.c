#include "hash.h"


index_t hash_str1(const char *key, size_t size)
{
        unsigned long hashval = 0;

        while (*key != '\0')
                hashval += *key++;

        return hashval % size;
}

// key has at least two characters
index_t hash_str2(const char *key, size_t size)
{
        return (key[0] + 27 * key[1] + 729 * key[2]) % size;
}

index_t hash_str3(const char *key, size_t size)
{
        unsigned long hashval = 0;

        while (*key != 0)
                hashval = (hashval << 5) + *key++;

        return hashval % size;
}
