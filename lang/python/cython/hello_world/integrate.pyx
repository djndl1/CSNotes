from collections.abc import Sequence
import cython

@cython.cfunc
@cython.ccall
def double(x: cython.double) -> cython.double:
    return x * 2

@cython.cfunc
@cython.ccall
def fib(n: cython.int):
    a, b = 0, 1
    result = [a, b]
    while b < n:
        a, b = b, a + b
        result.append(b)
        
    return result

# C function with a Python wrapper
cpdef long cfunc(long l):
    return l * 2

def primes(nb_primes: cython.int) -> Sequence[int]:
    i: cython.int
    p: cython.int[1000] # a C array on the stack
    
    if nb_primes > 1000:
        nb_primes = 1000
        
    if not cython.compiled:
        p = [0] * 1000      # zero the array
        
    len_p: cython.int = 0   # number of found primes
    n: cython.int = 2       # the number being examined
    
    while len_p < nb_primes:
        for i in p[:len_p]:
            if n % i == 0: # n is not a prime
                break
        else:  # the for-loop iterates over all i, n is thus a prime
            p[len_p] = n
            len_p += 1
        n += 1
            
    result_as_list = [prime for prime in p[:len_p]] # return as a Python list
    
    return result_as_list