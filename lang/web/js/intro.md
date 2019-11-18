# Basics

Javascript is embedded within `<script></script>` tag or as a file specified in `<script src="file.js"></script>`.

`use strict;` at the top of a file enforces strict modern features.

`let` declares a variable while `const` declares a constant (might not be assigned immediately). Some constants are named using uppercase.

Data types: `number`, `string`, `boolean`, `null`, `undefined`, `object`, `symbol`. Type conversions are done explicitly or implicitly. Numeric conversion happens in mathematical functions and expressions automatically (`+` concatenates strings though). When converting a string, whitespaces are trimmed first. `null` is converted to `0` and `undefined` to `NaN`. Values that are intuitively empty can be converted to `false`. Unary `+` converts other data types to `number`. `**` computes square. `++`/`--` can only used with variables. Bitwise operators treat arguments as 32-bit integer numbers. When comparing values of different types, JavaScript converts the values to numbers. `===`/`!==` checks the equality without type conversion. `null == undefined` evaluates to `true`. Equality check `==` and comparisons `< > <= >=` works differently. The latter convert `null` to a number. 

```javascript
null > 0; // false
null == 0; // false
null >= 0; // true, >= converts null.
```

Treat any comparison with `null`/`undefined` except the strict equality with exceptional care. Do not use comparisons with a variable which may be `null`/`undefined` casually.

`typeof` determines the data type. `null` returns `object`, which is an error in the languages.
