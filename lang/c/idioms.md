# skip to certain characters

```c
while (*s && *s != '/') s++;
```

# Allocate Memory for a Structure

This seems a more consistent style to malloc a structure.
The only downside is the variable has to be declared beforehand.

```c
MyStruct *d;
d = malloc(sizeof *d);
```

# Zero Initialization May Not Be a Good Thing

for debugging, zero value may not seem abnormal even if it is.
Set it to a broken value like `0xCD` to indicate an uninitialized value.
