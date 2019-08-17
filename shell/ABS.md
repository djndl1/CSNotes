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

- `$$`: process ID variable of the script itself

- `$!`: PID of last job running in background

```bash
eval 'kill -9 $!' &> /dev/null
```

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
scriptname >filename # redirects the output of `scriptname` to `filename`
command &>filename # redirects both stdout and stderr of command to filename
command >&2 # redirects stdout of command to stderr
scriptnam >>filename # appends the output of scripname to filename
[i]<>filename # opens filename for reading and writing and assigns file descriptor i to it. If filename does not exist, it is created.
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

## Variables and Parameters



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

- `$0`, `$1`, `$2`, ..., `${10}`, `$*`(a whole string), `$@` (each parameter is a quoted string): positional parameters with the final two denoting all the positional parameters and the first denoting the script's name; `$#`: the number of positional parameters, with `$0` not included.

The last argument is obtained using indirect reference:

```bash
args=$#
lastarg=${!args}
```

The `shift` command reassigns the positional parameters, in effect shifting them to the left one notch. A numerical parameter indicates how many positions to shift.

```bash
$1 <-- $2, $2 <-- $3, $3 <-- $4, ...
```

### Internal Variables

- `BASHPID`: Process ID of the current instance of Bash, not the same as `$$` (which returns the PID of the parent shell).

- `BASH_VERSINFO`: a 6-element array containing version information about Bash.

- `BASH_VERSION`: Bash version string 

- `EUID`: effective user ID, whatever identity the current user has assumed. not the same as `UID`.

- `UID`: current user's real id, even if temporarily assumed another identity through `su`.

- `FUNCNAME`: the current function name

- `GROUPS`: an array groups current user belong to

- `HOSTNAME`

- `HOSTTYPE`: identifies the system hardware

- `MACHTYPE`: machine type 

- `OSTYPE`: OS type

- `IFS`: internal field separator, determines how Bash recognizes fields, or word boundaries. Defaults to whitespace (space, tab and newline).

```bash
 djn  debian  ~  echo "$IFS"  | cat -vte
 ^I$ # single space, horizontal tab, newline
$
```

- `LINENO`: current line number, chiefly for debugging purposes.

- `OLDPWD`, `PWD`.

- `PPID`: parent PID

- `PS1`; `PS2`; `PS3`; `PS4`

- `SHELLOPTS`: enabled shell options

- `SECONDS`: the number of seconds the script has been running

```bash
rm .[A-Za-z0-9]*  # delete dotfiles
rm -f .[^.]* ..?* # remove filenames beginning with multiple dots
```

- `REPLY`: the default value when a variable is not supplied to `read`.

```bash
 djn  debian  ~  read
fadfa
 djn  debian  ~  echo $REPLY
fadfa
```

- `TMOUT`: Time out value. Logout after that.

### Typing variables

The `declare`/`typeset` permits modifying the properties of variables, a very weak form of typing.

- `-r`: readonly, `declare -r var1` = `readonly var1`;

```bash
 djn  debian  ~/FOSS/playground  declare -r a=5

 djn  debian  ~/FOSS/playground  a=3
-bash: a: readonly variable
```

- `-i`: integer, trying to assign a string to it will end up getting a `0`.

- `-a`: array

- `-f`: function

- `-x`: export, available for exporting outside the environment of the script itself ; `-x var=$val`

Also, `declare` restricts a variable's scope. If no name is given, `declare` displays the attributes and values of all variables.

### Random integer `$RANDOM`

`$RANDOM` is an internal Bash function that returns a pseudorandom itneger in the range 0 - 32767

Mod a range to limit its upper bound.

```bash
# generate a binary truth value
BINARY=2
number=$RANDOM
let "number %= $BINARY"
```

More usage :TODO

### Manipulating Strings

Bash supports a number of number manipulation operations, though inconsistent and overlapping. Some are a subset of parameter substitution and others fall under the functionality of the UNIX `expr` command.

- `${#string}`; `expr length $string`; `expr "$string" : '.*'` (returns the number of chars matched): get string length

- `expr match "$string" '$substring'`; `expr "$string" : '$substring'`: length of matching sbustring at beginning of string

- `expr index $string $substring`: position of the first char of  `substring` in `string` that matches.

- `${string:position}`; `{string:position:length}`; : string extraction. The position and length arguments can be parameterized and the position can be parenthesized negative (from the right end). Also, it can be used to extract positional parameters.

```bash
echo $(*:2) # the second and following
echo $(@:2) # same as above
echo $(*:2:3) # #2 #3 #4 three positional parameters
```

- `expr substr $string $position $length`; 

- `expr match "$string" '\($substring\)'`; `expr match "$string" '\($substring\)'`: extract from the beggining of `string`

- `expr match "$string" '.*\($substring\)'`; `expr "$string" : '.*\($substring\)'`: extract from the end of `string`

- `{string#substring}`: deletes shortest match of `substring` from front of `string`; `${string##substring}`: deletes longest match of `substring` from front of `string`.

- `{string%substring}`: deletes shortest match of `substring` from back of `string`; `${string%%substring}`: deletes longest match of `substring` from back of `string`.

- `${string/substring/replacement}`: replace the first match; `${string//substring/replacement}`: replace all matches; `${string/#substring/replacement}`: match from front and replace; `${string/%substring/replacement}`: match from back and replace.

A Bash script may invoke the string manipulation facilities of `awk` as an alternative to using its built-in operations.

## Parameter Substitution

- `${parameter}`: may be used to concatenating variables with strings

```bash
echo ${USER}-${HOSTNAME}
//djn-debian
```

- `${parameter-default}`, `${parameter:-default}` (`:` make a difference only when `parameter` has been declared but is null): if `parameter` not set (`:` adds null), _return_ `default`.

```bash
$ echo ${abd-$USER}
djn
$ echo ${HOME-$USER}
/home/djn

 djn  debian  ~  abd=

 djn  debian  ~  echo ${abd-$USER}


 djn  debian  ~  echo ${abd:-$USER}
djn
```

The default parameter construct finds use in providing missing comman-line arguments in scripts.

- `${parameter=default}`: if parameter not set, set it to default; `${parameter:=default}`: if parameter not set or null, _set it to default.

- `${parameter+alt_value}`: if parameter set, use `alt_value`, else use null string; `${parameter:+alt-value}`: if parameter set and not null, use `alt-value`, else use null string.

- `${parameter?err_msg}`: if parameter set, use it, else print `err_msg` and abort the script with exit status of 1.; `${parameter:?err_msg}`: if parameter set and not null, above.

- `${#array[*]}`/ `${#array[#]}`: the number of elements in the array.

- `${!varprefix*}`, `${!varprefix@}`: matches names of all previously declared variables beginning with `varprefix`.

## Quoting

Quoting has the effect of protecting special character in the string from reinterpretation or expansion by the shell or shell script.

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

## Exit

An `exit` with no parameter, the exit status of the script is the exit status of the last command executed in the script.

`$?` reads the exit status of the last command executed. A `$?` following the executation of a pip gives the exit status of the last command executed.

## Tests

An `if/then` construct tests whether the exit status of a list of commands of is 0.

`[` (a command)  is a synonym for `test`. `[[...]]` is the _extended test command_ where `[[` is a keyword.

`((...))` and `let...` constructs return an exit status according to whether the arithmetic expressions they evaluate expand to a nonzero value. If the last ARG evaluates to 0, let returns 1;  returns 0 otherwise.

The exit status of an arithmetic expression is not an error value.

An `if ` can test any command, not just conditions enclosed within brackets.

```bash
if cmp a b &> /dev/null
then 
if cmp a b &> /dev/null
then
    echo "Files a and b are identical"
else
    echo "Files a and b differ"
fi
```

```bash
if echo "$word" | grep -q "$letter_sequence"
then
    echo "$letter_sequence found in $word"
else
    echo "$letter_sequence not found in $word"
fi
```

Note the differnce between `0` `1` `-1`  and `[ 0 ]`, `[ 1 ]`, `[ -1 ]`. The latter three all evaluate to true. 

When `if` and `then` are on the same line in a conditional test, a semicolon must terminate the `if` statement. Both `if` and `then` are keywords, which themselves begin statements. `[` doesn't necessarily requires `]`, however, newer versions of Bash requires it. There are builtin `[`, `/usr/bin/test` and `/usr/bin/[`. They are all the same.

`[[]]` construct is the more versatile Bash version of `[]`. Using the `[[ ... ]]` test construct, rather than `[ ... ]` can prevent many logic errors in scripts. For example, the `&&`, `||`, `<`, and `>` operators work within a `[[ ]]` test, despite giving an error within a `[ ]` construct. Arithmetic evaluation of octal / hexadecimal constants takes place automatically within a `[[ ... ]]` construct.

```bash
if [[ 15 -eq 0x0f ]] // [] error
then
    echo "Equal"
else
    echo "NotEqual"
fi
# Equal
```

A condition within test brackets may stand alone without an `if`, when used in combination with a list construct.

```bash 
[[ 15 -eq 0xfd ]] && echo "Equal" # Equal
```


Arithmatic expansion has the property of returning an exit status 0 when evaluating to nonzero, which is exactly what `if` needs.

```bash
if (( "5 > 2" ))
then
    echo "5>2"
else
    echo "5<=2"
    
fi
```

### Important file test operators

- `-e`/`-a`: file exists

- `-f`: regular files

- `-s`: not zero size

- `-d`: directory file

- `-b`: block file

- `-c`: character file

- `-p`: pipe file

```bash
echo "Input" | [[ -p /dev/fd/0 ]] && echo PIPE || echo STDIN
PIPE
```

- `-h`, `-L`: symbolic link

- `-S`: socket

- `-t`: file (descriptor) is associated with a terminal device

- `-r`;`-w`;`-x`: read/write/execute permission

- `-g`: set-group-id, a file within such a directory belongs to the group that owns the directory, not necessarily th the group of the user who created the file. This may be useful for a directory shared by a workgroup.

- `-u`: set-user-id, a binary owned by root with this flag runs with root priviledges, even when an ordinary user invokes it.

- `-k`: sticky bit, if set on a file, it's kept in cache memory; if set on a directory, it restricts write permission. This restricts altering or deleting specific files in such a directory to the owner of those files.

- `-O`: are you the owner?

- `-G`: your group?

### integer comparison

- `-eq`; `-ne`; `-gt`; `-ge`; `-lt`; `-le`;

- `<`; `<=`; `>`; `>=` only within `[[ ]]`

### String comparison

- `=`; `==`; `==` behaves diffferently within a double-bracket test than within single brackets

```bash
[[ $a == z* ]] # True if $a starts with an "z" (pattern matching).
[[ $a == "z*" ]] # True if $a is equal to z* (literal matching).
[ $a == z* ]  # File globbing and word splitting take place.
[ "$a" == "z*" ] # True if $a is equal to z* (literal matching).
```

- `!=`; `<`; `>`; the latter two needs an escape in `[ ]`

- `-z`: null string

- `-n`: not null string, always quote a tested string; the `[...]` test alone detects whether the string is null

### compound comparison

- `exp1 -a exp2`: logical and, or `[[ condition1 && condition2 ]]` (short-circuit)

- `exp1 -o exp2`: logical or, or `[[ conditional1 || condition2 ]]` (short-circuit)

Condition tests using the `if/then` may be nested.

## Operators

- `=`: all purpose assignment operator, which works for both arithmetic and string assignment

- `+`; `-`; `*`; `/`; `**` (exponentiation); `+=`; `-=`; `*=`; `/=`; `%=`;

```bash
let "n = $n + 1"
: $((n = $n + 1))
(( n = n + 1))
n=$(($n+1))
: $[ n = $n + 1]
n=$[$n+1]

let "n++"
: $((n++))
: $[n++]
((n++))
```

Bash integers are now 64-bit long. Bash does not understand floating point arithmetic. It treats numbers containing a decimal point as strings.

- bitwise operator: `<<`; `<<=`; `>>`; `>>=`; `&`; `&=`; `|`; `|=`; `~`; `^`; `^=`;

- logical operator: `!`; `&&`; `||`

```bash
if [ $condition1 ] && [ condition2 ]
if [ $condition1 -a $condition1 ]
if [[ $condition1 && $condition1 ]]
# same for || 
```

The comma operator chains together two or more arithmetic operations and returns the last one.

```bash
let "dec=32" # base 10
let "oct=032" # base 8, 26
let "hex=0x32" # base 16, 50

# BASE#NUMBER, where BASE is between 2 and 64, 10 digits + 52 characters (lower and upper) + @ + _
let "bin= 2#10100110111" # base 2
let "b32 = 32#77" # base 32
```

- C-style `++`, `--` also work. Ternary operator `condition ? a : b` also works.

# Loops and Branches

## Loops 

### `for`-loop
`for arg in [list]`: the basic looping construct.

```bash
for arg in [list] # may contain wild cards, entire list enclosed in quotes creates a single variable
do
    commands...
done
```

Omitting the `in [list]` part causes the loop to operate on `$@`.

`seq` is a useful range command when using with `for`-loop, or use `{m..n}`

```bash
for a in {1..10}
for a in `seq 10`
```

It is possible to use C-like `for`-loop:

```bash
for ((a=1; a <= LIMIT ; a++))
do
    echo -n "$a"
done
```

`do` and `done` can even be replaced by curly brackets in certain contexts

```bash
for ((n=1; n<=10; n++))
{
    echo -n "$n "
}
```

### `while`-loop

```bash
while [ condition ]
do
    commands
done
```

A `while`-loop may have multiple conditions. Only the final condition determines when the loop  terminates.

```bash
var1=unset
previous=$var1
while echo "previous-variable = $previous"
      echo
      previous=$var1
      [ "$var1" != end ]
do
echo "Input variable #1 (end to exit) "
    read var1
    echo "variable #1 = $var1"
done
```

A `while`-loop may employ C-style syntax  by using the double-parentheses construct.

```bash
((a = 1))
while ((a <= LIMIT))
do
    echo -n "$a "
    ((a+=1))
done
```

Inside its test brackets, a `while`-loop can call a function

```bash
t=0
condition ()
{
    ((t++))
    if [ $t -lt 5 ]
    then
        return 0 # true
    else
        return 1 # false
    fi
}
while condition
do
    echo "Still going: t = $t"
done
```

`while` has similar behavior of condition test to `if`

```bash
while read line
do
    ...
done
```

### `until`-loop

```bash
until[ condition is true ]
do 
    commands
done
```

An `until`-loop permits C-like test constructs

```bash
until [ "$var" = "end" ]
do
    read var
    echo "var = $var"
done

until (( var > LIMIT ))
do
    echo -n "$var "
    ((var++))
done
```

Bash `for`-loop is more loosely structured and more flexible than its equivalent in other languages. Therefore, feel free to use whatever type of loop gets the job done in the simplest way.

## Loop Control

`break` and `continue` loop control commands correspond exactly to their counterparts in other programming languages. `break` may optionally take a parameter to break out of N levels of loop. A `continue N` terminates all remaining iterations at its loop and continues with the next iteration at the loop N levels above (however, it's tricky to use in any meaningful context, better to avoid).

```bash
for outer in I II III IV V
do
    echo; echo -n "Group $outer"
    for inner in `seq 10`
    do
        if [[ "$inner" -eq 7 && "$outer" = "III" ]]
        then
            continue 2
        fi
        
        echo -n "$inner " # 7 8 9 10 will not echo on "Group III."
    done
done
```

## Testing and Braching

### `case`

```bash
case "$var" in
    "$condition1")
    commands...
    ;;
    
    "$condition2")
    commands...
    ;;
esac
```

```bash
case "$Kerpress" in 
    [[:lower:]] ) echo "lowercase"
    [[:upper:]] ) echo "uppercase"
    [0-9] ) echo "Digit"
    * ) echo "Punctuation, whitespace, or other"
esac
```

A use of `case` involves testing for command line parameters.

```bash
while [ $# -gt 0 ]
do
    case "$1" in
        -d|--debug)
                DEBUG=1
                ;;
        -c|--conf)
                CONFFILE="$2"
                shift
                if [ ! -f $CONFFILE ]; then
                    echo "Error: Supplied file doesn't exist!"
                    echo 2
                file
                ;;
    esac
    shift
done
```

### `select`

`select` prompts the user to enter one of the choices presented in the variable list.

```bash
select var [ in list ]
do
    commands
    break
done
```

If `in list` is ommited, then `select` uses the list of command line arguments passed to the script or the function containing the `select` construct.

# Command Substitution

Command substitution reassigns the output of a command or even multiple commands; it literally plugs the command output into another context.

```bash
`command` # classic form
$(command) # alternative form
```

Command substitution invokes a subshell. Command substitution may result in word splitting. You may quote it. However, this may causes trailing newlines. Using `echo` to output an unquoted variable set with command substitution removes trailing newlines characters from the output of the reassigned commands.

```bash
echo `ls -lh`
echo "`ls -lh`"
```

Command substitution permits setting a variable to the contents of a file using either redirections or the `cat` command. However, this is not recommended.

```bash
variable1=`<file1`
variable2=`cat file2`
```

Command substitution permits setting a variable to the output of a loop.

```bash
a="`for i in $(seq 10); do echo $((i++)); done`"
echo $a
1 2 3 4 5 6 7 8 9 10
```

The `$(...)` form permits nesting.

# Arithmetic Expansion

```bash
z=`expr $z + 3` # not recommended
z=$(($z+3))
z=$((z+3))
let z=z+3
let "z += 3"
```

# Commands

Mastering the commands is an indispensable prelude to writing effective shell scripts.

## Internal Commands and Builtins

A builtin execute faster than external commands that usually require forking off a separate process. A builtin may be a synonym to a system command of the same name, but Bash reimplements it internally like `echo`. 

- `echo`: normally, each `echo` command prints a terminal newlne, `-n` suppresses this. `echo `command` ` deletes any linefeeds that the output of command generates.

- `printf`: fromatted print, limited variant of the C language `printf()`. Formatting error messages is a useful application of `printf`.

- `read`: reads the value of a variable from `stdin`. The `-a` option gets array variables. Without associated variables, the input is assigned to `$REPLY`. `\` in the input suppresses a newline, `-r` causes `\` to be treated literally.

More usage TODO

- `cd`

- `pwd`

- `pushd`, `popd`, `dirs`: a mechanism for bookmarking working directories. `$DIRSTACK` variable related. Scripts that require various changes to the current working directory without hard-coding the directory name changes can make good use of the mechanism.

- `let`: carries out arithmetic operations, it functions as a less complex version of `expr`.

- `eval arg1 [arg2] ... [argN]`:  combines the argument in an expression or list of expressions and evaluates them. Any variables within the expression are expanded. The eval utility shall construct a command by concatenating arguments  together, separating each with a <space> character.

- `set`: changes the value of internal script variables/options. One use is to toggle option flags which help determine the behavior of the script. Another application is to reset the positional parameters. Invoking `set` without arguments or options lists all the environment variables and other variables that have been initialized. 

```bash
set `uname -a` # sets the positional parameter to the output of the command `uname -a`
set -- $var    # sets the contents of var to positional parameters
set --         # unsets all positional parameters
```

- `unset`: deletes a shell variable, setting it to null. This command does not affect positional parameters.

- `export`: makes available variables to all child processes of the running script or shell.

- `getopts`: parses command-line arguments passed to the script. It uses two implicit variables `$OPTIND` and `$OPTARG`.

```bash
while getopts ":abcde:fg" Option
do
    case $Option in
        a)
            echo "Option a $OPTARG"
           ;;
        b)
            echo "Option b"
            ;;
        c)
            echo "Option c"
            ;;
        d)
            echo "Option d"
            ;;
        e)
            echo "Option e $OPTARG"
            ;;
        f)
            echo "Option f"
            ;;
        g)
            echo "Option g"
            ;;
        *)
            echo "Strange args"
            ;;
    esac
done
```

- `source`: sourcing a file imports code into the script. If the sourced file is itself an executable script, then it will run, then return control to the script that called it. A sourced executable script may use a `return` for this purpose.

- `exit`: unconditionally terminates a script. It is good practice to end all but the simplest script with an `exit 0`.

- `exec`: replaces the current process with a specified command. The shell does not fork and the command `exec`ed replaces the shell. It forces an exit from the script when the `exec`ed command terminates.

- `shopt`: changes shell options on the fly. It often appears in the Bash startup files.

- `caller`: echoes to `stdout` information about the caller of that function

- `true`/`false`: returns a successful(zero)/unsuccessful exit status but does nothing else.

- `type`: can be useful for testing whether a certain command exists.

- `hash`: records the path name of specified commands in the shell hash table so the shell or script will not need to search the `$PATH` on subsequent calles to those commands.

- `bind`: displays or modifies `readline` key bindings.

- `help`

### Job Control

https://unix.stackexchange.com/questions/3886/difference-between-nohup-disown-and

http://linuxcommand.org/lc3_lts0100.php

- `jobs`: listing the jobs running in the background, giving the job number.

- `disown`: remove jobs from the shell's table of active jobs

read [Difference Between nohup disown](https://unix.stackexchange.com/questions/3886/difference-between-nohup-disown-and)

- `fg`: switches a job running in the background into the foreground; `bg`: restarts a suspended job and runs it in the background.

- `wait`: suspend script executation unitl all jobs in background have terminated or until the job number or process ID specified as an option terminates.  `wait` may be used to prevent a script from exiting before a background job finishes.'

- `suspend`: similar effect to `Ctrl-Z`, it suspends the shell.

- `logout`

- `times`: give statistics on the system time elapsed when executing commands. Not common to profile and benchmark shell scripts.

- `kill`

- `killall`: an external command

- `command`: diasbles aliases and functions for the command immediately following it.

- `builtin command`: invoke a built-in command

- `enable`: enables or disables a shell builtin command

- `autoload`: a function withan `autoload` declaration will load from an external file at its first invocation. This saves system resources. Not a part of the core Bash installation.

## External Filters, Programs and Commands

- `ls`: `-R`, recursive; `-S`: sort by size; `-t`: sort by modification time; `-v`: sort by numerical version number embedded in the filenames; `-b`: show escape characters; `-i`: show file inodes.

- `cat`/`tac`: `tac` lists a file backwards from its end. `cat -n`: prepend a line number to every line in the output. `cat` is commonly used to concatenate files. In a pipe, it may be more efficient to redirect the `stdin` to a file rather than to `cat` the file.

- `rev`: reverse every line of a file.

- `cp`: `-a`: archive flag for copying an entire directory tree; `-u`: update flag which prevents overwriting identically-named newer files. `-r`/`-R`: recursive flags

- `mv`: `-f`: do not prompt before overwriting.

- `mkdir -p`: automatically creates any necessary parent directories.

- `chattr`/`lsattr`: change/list file attributes

- `find`: `-exec` carries out command on each file that find matches.

```bash
find . -maxdepth 1 -name '*.md' -exec lsattr {} \;
find "$DIR" -type f -atime +5 -exec rm {} \;
find /etc -exec grep '[0-9][0-9]*[.][0-9][0-9]*[.][0-9][0-9]*[.][0-9][0-9]*' {} \;
```

More usage TODO

- `xargs`: a filter for feeding arguments to a command and also a tool for assembling the commands themselves. It reads items from the stdin, delimited by blanks or newlines and executes the command with any initial arguments followed by items read from stdin by `xargs`.

```bash
ls | xargs -p gzip # gzips every file in pwd
```

The `-P` option to `xargs` permits running processes in parallel. A curly bracket servers as a placeholder for replacement text.

```bash
ls | xargs -i echo {} # two echoes instead of one
ls | xargs -i wc -l {}
```

### Date/Time

- `date`: date and time

- `zdump`: echoes the time in a specified time zone

```bash
zdump UTC-8
zdump EST
```

- `time`: output verbose timing statistics for executing a command

- `touch`: update access/modification time, also creates a file if none

tip: use `touch` to stop `cp -u` overwriting a file.

- `at`/`batch`: batch job

- `cal`: calendar

- `sleep`

- `usleep`: micro sleep in microseconds

- `hwclock`, `clock`: accesses or adjusts the machine's hardware clock.

### Text  Processing 

TODO

### File and Archiving

TODO

### Communication 

TODO

### Terminal Control

TODO

### Math

TODO

### Miscellaneous

TODO

## System and Administrative Commands


