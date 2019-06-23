# Regular expression in Python

Both patterns and strings to be searched can be Unicode strings (`str`) as well as 8-bit strings (`bytes`). However, Unicode strings and 8-bit strings cannot be mixed: that is, you cannot match a Unicode string with a byte pattern or vice-versa; similarly, when asking for a substitution, the replacement string must be of the same type as both the pattern and the search string.

`\` is used to indicate special forms, literal backslash have to be written `\\`.

Python's raw string notation is used for regular expression patterns.

## Syntax

### Special characters

- `.` (dot): matches any character except a newline unless `DOTALL` flag is present

- `^`(caret): the start of the string

- `$`: the end of the string.

- `*`: 0 or more repetitions of the preceding Regex.

- `?`: 0 or 1 repetitions of the preceding Regex.

- `{m}`: match exactly `m` copies of the previous regex.

- `{m,n}`: match from `m` to `n` repetitions of the preceding regex.

- `[]`: a set of characters. complementing can be done using `^` inside the braces.

- `|`: OR-match 

- `(...)`: match whatever regx is inside the parentheses.

...


