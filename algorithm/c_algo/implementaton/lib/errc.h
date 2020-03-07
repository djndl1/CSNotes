#pragma once

#include "utils.h"

enum errc {
    no_error = 0,
    null_pointer_error,
    out_of_range,
};

typedef enum errc err_t;

#define define_result_t(type) \
    typedef struct { \
    err_t err; \
    type result; \
    } CONCAT(type, _result_t)

#define define_modifier_result_t(modifier, type)                               \
  typedef struct {                                   \
    err_t err;                                                                 \
    modifier type result;                                                      \
  } modifier##_##type##_result_t

#define define_pointer_result_t(type) \
    typedef struct { \
    err_t err;       \
    type * result; \
    } type##_pointer##_result_t

#define result_t(type) \
    CONCAT(type, _result_t)

define_result_t(int);
define_result_t(unsigned);
typedef result_t(unsigned) unsigned_int_result_t;


define_result_t(short);
typedef result_t(short) short_int_result_t;
define_modifier_result_t(unsigned, short);

define_result_t(long);
typedef result_t(long) long_int_result_t;
define_modifier_result_t(unsigned, long);

#if __STDC_VERSION__ > 199901L
#include <stdint.h>
#include <stddef.h>

typedef result_t(int) result_t(bool);
typedef result_t(int) result_t(_Bool);

define_result_t(int8_t);
define_result_t(int16_t);
define_result_t(int32_t);
define_result_t(int64_t);

define_result_t(uint8_t);
define_result_t(uint16_t);
define_result_t(uint32_t);
define_result_t(uint64_t);

define_result_t(size_t);
define_result_t(ptrdiff_t);

#endif // C99

#if __STDC_VERSION__ > 201112L
define_result_t(max_align_t);
#endif

define_result_t(float);
define_result_t(double);

