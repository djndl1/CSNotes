`java.math.BigInteger` and `java.math.BigDecimal` are classes for manipulating numbers with an arbitrary long sequences of digits.

`+` `*` are not available since java has not operator overloading.

# Class `java.math.BigInteger`

immutable arbitrary precision integer. BigInteger provides analogues to all of Java's primitive integer operators and all relevant methods from `java.lang.Math`. Additionally, `BigInteger` provides operations for modular arithmetic, GCD calculation, primality testing, prime generation, bit manipulation and a few other miscellaneous operations. 

- static `.valueOf()`: return a `BigInteger` whose value is equal to that  of the argument


# Class `java.math.BigDecimal`

Immutable, arbitrary-precision signed decimal numbers. A BigDecimal consists of an arbitrary precision integer unscaled value and a 32-bit integer scale. If zero or positive, the scale is the number of digits to the right of the decimal point. If negative, the unscaled value of the number is multiplied by ten to the power of the negation of the scale. The value of the number represented by the BigDecimal is therefore (unscaledValue × 10^-scale).

The BigDecimal class provides operations for arithmetic, scale manipulation, rounding, comparison, hashing, and format conversion.
