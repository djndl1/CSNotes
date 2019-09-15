#pragma once

#include <stddef.h>

typedef size_t index_t;

#define hash_int(x, size) (x % size)

index_t hash_str1(const char *key, size_t size);

index_t hash_str2(const char *key, size_t size);

index_t hash_str3(const char *key, size_t size);

