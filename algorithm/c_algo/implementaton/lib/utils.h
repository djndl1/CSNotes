#pragma once

#define CONCAT(a, b) a##b

#define for_range(lower, upper, idx)                \
    for (size_t idx = lower; idx < upper; idx++)

#define SIZE(container) (container.sz)
#define AT(container, pos) (container.data[pos])

#define for_each_in(idx, container)                  \
    for (size_t idx = 0; idx < SIZE(container); idx++)

#define for_each_do(container, func) \
    for (size_t _ = 0; _ < SIZE(container); _++) \
        func(&(AT(container, _)))

#define BEGIN(type, container) ((type*)((container).data))
#define END(type, container) ((type*)((container).data) + (container).sz)
#define DATA(container) ((container).data)
#define EMPTY(container) ((container).size == 0)

#define PUSH_BACK(value, container) \
    if ((container).push_back) (container).push_back(&(container), value)


#define for_each(type, ptr, container)           \
    for (type *ptr = BEGIN(type, container); ptr < END(type, container); ptr++)

#define DELETE(var) \
    ((var).dtor(&(var)))


