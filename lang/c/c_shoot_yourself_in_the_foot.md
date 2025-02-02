- casting a `GtkButton` to a callback resulted in a segfault.

[strict aliasing](https://stackoverflow.com/questions/98650/what-is-the-strict-aliasing-rule)

https://cellperformance.beyond3d.com/articles/2006/06/understanding-strict-aliasing.html

What is `restrict`: https://stackoverflow.com/questions/19731256/what-is-the-level-of-strict-aliasing-implied-by-the-restrict-keyword

# Dealing with C String

https://www.approxion.com/dangerously-confusing-interfaces-iv-the-perils-of-cs-safe-string-functions/

# Pointers and Arrays are Different

An array may decay to a pointer but it is not an array.

```c
int p* = { 0 }; // a NULL is assigned to p
int p[] = { 0 }; // a proper { 0 } array
```
