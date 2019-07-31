In the simplest case, a script is nothing more than a lsit of system commands stored in a file.

she-bang is actually a two-byte magic number, see `man magic`.

# Basics

- `;;`: terminator in a case option

- `,`: comma operator links together a series of arithmetic operations. All are evaluted, but only the last one is returned.

```shell
let "t2 = ((a = 9, 15 / 3))"
```
The comma can also concatenate strings.

- `"'`: lowercase conversion in parameter substitution

- ```: command substitution, also known as backquotes, or backticks.

- `:`: null command, NOP. Its exit status is true, thus can be considered a synonym for the shell builtin true.

```bash
while : // while true
do 
    op-1
    op-2
    ...
    op-n
done
```

```bash
if condition
then : # do nothing
else
    take-some-action
fi
```

Also a placeholder where a binary operation is expected.

```bash
: ${username=`whoami`}
: ${1?"Usage: $0 ARUGMENT"}

: ${HOSTNAME?} ${USER?} ${MAIL?}
: > data.xxx // truncate a file same as `cat /dev/null > data.xxx` without forking a process
: >> target.xxx // create if not present, otherwise nothing
```

Also a field separator, as in `$PATH` or in  `/etc/passwd`.

- `!`: reverse or negate the sense of a test or exit status [bang]. Or indirect variablre references.

- `*`: wildcard in globbing; `?`: single character in globbing.

- `$`: variable substitution.

- `${}`: parameter substitution.

- `$'...'`: quoted string expansion.

- `$*`, `$@`: positional parameter.

- `$?`: exit status

- `$$`: process ID variable.

- `()`: command group; a listing of commands within parentheses starts a subshell.

```bash
(a=hello; echo $a)
```

- `{xxx,yyy,zzz}`: brace expansion

``` bash
> echo ab{1,2,3,4}cd
ab1cd ab2cd ab3cd ab4cd
```

- `{a...z}`: extended brace expansion

```bash
> echo a{1..3}b
a1b a2b a3b
```

- `{}`: block of code; inline group, which creates an anonymous function. The variable inside a code block remain visible to the remainder of the script.

- `[]`: test

- `[[]]`: test

- `(())`: integer expansion, expand and evaluate integer expression within.

- `> &> >& >> < <>`: redirection;

```bash
scriptname > filename # redirects the output of `scriptname` to `filename`
command &> filename # redirects both stdout and stderr of command to filename
command >&2 # redirects stdout of command to stderr
scriptnam >> filename # appends the output of scripname to filename
[i]<> filename # opens filename for reading and writing and assigns file descriptor i to it. If filename does not exist, it is created.
```

- `<` `>` ascii comparison

```bash
if [[ "$veg1" < "$veg2" ]]
then
    ...
else
    ...
fi
```

- `<<`: redirectionin a here document

- `<<<`: redirection in a here string

- `|`: A pipe runs as a child process, and therefore cannot alter script variables. If one of the commands in the pipe aborts, this prematurely terminates execution of the pipe, called a _broken pipe_, sending a `SIGPIPE` signal.

- `>|`: force redirection.

- `-`: redirection from/to stdin or stdout, not a bash builtin. Where a filename is expected, `-` redirects output to stdout or accepts input from stdin.

```bash
 $ file -
#!/usr/bin/env python
/dev/stdin: Python script, ASCII text executable
```

- `~+`: current working directory, `$PWD`

- `~-`: previous working directory, `$OLDPWD`, ???

## Control characters

`Ctl-G`: bell.

`Ctl-H`: rubout, destructive backspace

`Ctl-J`: line feed

`Ctl-K`: vertical tab. Within a script, vertical tab goes straight down.

`Ctl-I`: horizontal tab

`Ctl-U`: kill backwards to the beginning or the whole line

`Ctl-M`: carriage return

`Ctl-L`: formfeed

`Ctl-O`: issue a newline

`Ctl-R`: backwards search for text in history buffer

`Ctl-S`: suspend

`Ctl-Q`: resume

`Ctl-V`: inserts control characters

`Ctl-T`: swap the current char with the previous one

`Ctl-W`: kill a word backwards

# Variables and Parameters



`$VAR` is a simplified form of `${VAR}`. Undeclared/uninitialized variable has a null value. Quoted strings exists as a whole.

```bash
a=15+5     # a 15+5
let b=20+1 # b 21
read a     # implicitly set a

var= # null value
unset var  # unset it
```

A null-valued variable is not the same as unsetting it.

Bash variables are untyped. Bash does not segregate its variables by type. Essentially, Bash variables are character strings. Depending on context, Bash permits arithmetic operations and comparsions on variables. The determining factor is whether the value of a variable contains only digits.

```bash
a=2345
let "a += 5" # a is now 2350
b=${a/23/BB} # However, it's still a string and can be substituted.
declare -i b # declaring it an integer doesn't help
let "b += 1" # b is now 1, the integer value of a string is 0

e='' # null value is integer 0
```

- `local var`: variable visible only within a code block or function

- `Environmental var`: variables that affect the behavior or the shell or user interface

- `$0`, `$1`, `$2`, ..., `${10}`, `$*`/`$@`: positional parameters with the final two denoting all the positional parameters and the first denoting the script's name; `$#`: the number of positional parameters, with `$0` not included.

The last argument is obtained using indirect reference:

```bash
args=$#
lastarg=${!args}
```

The `shift` command reassigns the positional parameters, in effect shifting them to the left one notch. A numerical parameter indicates how many positions to shift.

```bash
$1 <-- $2, $2 <-- $3, $3 <-- $4, ...
```

# Quoting

Quoting has the effect of protecting special character in teh tring from reinterpretation or expansion by the shell or shell script.

When referencing a variable, it is generally advisable to enclose its name in double quotes, which prevents reinterpretation of all special charactes within the quoted string, except `$`, ```(backquote) and `\`(escape). Use double quotes to prevent word splitting.

`\b` is not the backspace on the keyboard, more like the left arrow.

`$'abc'` is string expansion.

```bash
$ echo $'afd\nbcd'
afd
bcd
```

```bash
    case "$key" in
        $'x\1b\x5b\x32\x7e')
            echo Insert Key
            ;;

        d)
            date
            ;;
        q)
            echo Time to quit...
            echo
            exit 0
    esac
```

```bash
echo "foo\
bar"
#foobar
```
