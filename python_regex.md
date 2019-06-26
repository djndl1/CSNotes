# Regex

## Syntax

### Special characters

- `.` (dot): matches any character except a newline unless `DOTALL` flag is present

- `^`(caret): the start of the string

- `$`: the end of the string.

- `*`: 0 or more repetitions of the preceding Regex. ! this is not wildcard `*`.

- `+`: 1 or more repetitions of the preceding regex.

- `?`: 0 or 1 repetitions of the preceding Regex.

- `{m}`: match exactly `m` copies of the previous regex.

- `{m,n}`: match from `m` to `n` repetitions of the preceding regex.

- `[]`: a set of characters. complementing can be done using `^` inside the braces.

- `|`: OR-match 

- `(...)`: match whatever regx is inside the parentheses.

...

Some of the special sequences beginning with `\` represent predefined sets of charactersthat are often useful.

- ``\d` = `[0-9]`: digit

- `\D` = `[^0-9]`: nondigit

- `\s`= `[\t\n\r\f\v]`: whitespace

- `\w`: alphanumeric

- `\W`: non-alphanumeric

# Regular expression in Python

Regex patterns are compiled into a series of bytecodes which are then executed by a matching engine written in C.

Both patterns and strings to be searched can be Unicode strings (`str`) as well as 8-bit strings (`bytes`). However, Unicode strings and 8-bit strings cannot be mixed: that is, you cannot match a Unicode string with a byte pattern or vice-versa; similarly, when asking for a substitution, the replacement string must be of the same type as both the pattern and the search string.

`\` is used to indicate special forms, literal backslash have to be written `\\`.

Python's raw string notation is used for regular expression patterns.

The matching engine goes as far as it can at first, cand if no match is found it will then prograssively back up and retry the rest of the RE again and again.

