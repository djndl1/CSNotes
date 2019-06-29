# Regex

## Syntax

### Special characters

- `.` (dot): matches any character except a newline unless `DOTALL` flag is present

- `^`(caret): the start of the line

- `$`: the end of the line.

- `\A`: the start of the string

- `\b`: word boundary, zero match.

- `\B`: non-word boundary

- `*`: 0 or more repetitions of the preceding Regex. ! this is not wildcard `*`.

- `+`: 1 or more repetitions of the preceding regex.

- `?`: 0 or 1 repetitions of the preceding Regex.

- `*?`, `+?`, `??`, `{m,n}?` match as little text as possible, so called non-greedy

- `{m}`: match exactly `m` copies of the previous regex.

- `{m,n}`: match from `m` to `n` repetitions of the preceding regex.

- `[]`: a set of characters. complementing can be done using `^` inside the braces.

- `|`: OR-match 


## Grouping 

- `(...)`: Grouping whatever regex is inside the parentheses. groups can be nested.

- `\1`: backreference a former matched group

- `(?:whatever regex here)`: non-capturing group, not interested in retrieving the group's content. Particularly useful when added to an existing pattern without having to care about group numbering.

- `(?<name>whatever regex here)`: named groups, associating a group with a name.

- `(?P=name)`: match the so-named group

## Special characters

Some of the special sequences beginning with `\` represent predefined sets of charactersthat are often useful.

- `\d` = `[0-9]`: digit

- `\D` = `[^0-9]`: nondigit

- `\s`= `[\t\n\r\f\v]`: whitespace

- `\w`: alphanumeric

- `\W`: non-alphanumeric

## Lookahead zero-assertions

- `(?=...)`: positive lookahead assertion, succeeds if the contained regular expression represented by `...` matches at the current location. Once matched, the matching engine doesn't advance at all; the rest of the pattern is tried right where the assertion started.

- `(?!...)`: negative lookahead assertion, succeeds if the contained expression doesn't match at the current postion in the string.

`.*[.](?!bat$|exe$)[^.]*` matches any file names except those ending in `.exe` or `.bat`.

## Compilation flags

Compilation flags modify some aspects of how regular expressions work, denoted by a long name or a short name.

`ASCII`/`A`; `DOTALL`/`S`; `IGNORECASE`/`I`; `LOCALE`/`I`; `MULTILINE`/`L`; `VERBOSE`/`X`;

# Regular expression in Python

Regex patterns are compiled into a series of bytecodes which are then executed by a matching engine written in C.

Both patterns and strings to be searched can be Unicode strings (`str`) as well as 8-bit strings (`bytes`). However, Unicode strings and 8-bit strings cannot be mixed: that is, you cannot match a Unicode string with a byte pattern or vice-versa; similarly, when asking for a substitution, the replacement string must be of the same type as both the pattern and the search string.

`\` is used to indicate special forms, literal backslash have to be written `\\`. Since a literal backslash must be expressed as `\\` in RE, the string representation of RE `\\` then is expressed as `\\\\` or `r'\\'`.

Python's raw string notation is used for regular expression patterns.

The matching engine goes as far as it can at first, cand if no match is found it will then prograssively back up and retry the rest of the RE again and again.

`re` module is not always the right choice especially when string methods provide enough functionality.

# Module content

## Compilation

Regular expressions are compiled int pattern objects, which have methods for various operations.

- `re.compile(pattern, falgs=0)`: compile (RE is not part of python) a regular expression pattern into a regex object used for matching.

## matching

- `re.search(pattern, string, flags=0)`: Scan through string looking for the first location where the regular expression pattern produces a match, and return a corresponding match object.

- `re.match(pattern, string, flags=0)`: If zero or more characters at the beginning of string match the regular expression pattern, return a corresponding match object. 

## Splitting

- `re.split(pattern, string, maxsplit=0, flags=0)`: Split string by the occurrences of pattern. If capturing parentheses are used in pattern, then the text of all groups in the pattern are also returned as part of the resulting list.

## Substitution

`re.sub(pattern, repl, string, count=0, flags=0)`: Return the string obtained by replacing the leftmost non-overlapping occurrences of pattern in string by the replacement repl. repl can be a string or a function. The pattern may be a string or a pattern object.

`re.subn(pattern, repl, string, count=0, flags=0)`: Perform the same operation as sub(), but return a tuple `(new_string, number_of_subs_made)`.

## Class `Pattern`

Compiled regular expression  objects support similar methods to those functions above.

## Class `Match`

Match objects always have a boolean value of True. No matching returns a `NoneType`.

`Match.group([group1, group2, ...])`: returns one or more subgroups of the match. If a groupN argument is zero, the corresponding return value is the entire matching string
