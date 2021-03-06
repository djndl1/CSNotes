# Python Lexical Analysis

A stream of taoken, read as Unicode code points, by a parser, generated by the lexical analyzer.

### Line structure

logical lines

physical lines: terminated by an end-of-line 

#### Encoding declarations

```python
# -*- coding: <encoding-name> -*-
```

recognized by GNU emacs

and 

```python
# vim:fileencoding=<encoding-name>
```

recognized by VIM

otherwise default encoding is UTF-8.

### Line Joining

explictly using `\` to form a single logical line

or implicitly expressions in parentheses, square brackets or curly braces can be split over more than one physical line without using backslashes.

## identifiers and Keywords

Besides NEWLINE, INDENT and DEDENT, the following categories of tokens exist: identifiers, keywords, literals, operators, and delimiters.

## Literals

Multiple adjacent string or bytes literals (delimited by whitespace), possibly using different quoting conventions, are allowed, and their meaning is the same as their concatenation. Thus, "hello" 'world' is equivalent to "helloworld".
