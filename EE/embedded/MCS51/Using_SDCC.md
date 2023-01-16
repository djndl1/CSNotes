# Toolchain

## SDCC

- A host of standard optimization

- Inline Assembly

### Components

- `sdcc`: the compiler

- `sdcpp`: C preprocessor

- `sdas*`: assemberls

- `sdld`: the linker for 8051 and STM8

- simulators

- `sdcdb`: the source debugger

- `sdcclib`: deprecated

- `sdar, sdranlib, sdnm, sdobjcopy`: archive managing and indexing utilities

- `packihx`: pack/compress Intel hex files

- `makebin`: convert Intel Hex file to a binary

- `<installdir>/share/sdcc/include`: the include files

- `<installdir>/share/sdcc/lib`: the precompiled relocatables

### Data Types

- `_Bool`/`bool`: 8 bits, unsigned

- `char`: 8 bits, unsigned

- `short`: 16 bits

- `int`: 16 bits

- `long`: 32 bits

- `long long`: 64 bits

- `float`: 4 bytes similar to IEEE754

- `pointer`: 1, 2, 3 or 4 bytes generic

- `__bit`: 1 bit unsigned

### Standard Compliance

- C90: `--std-c89`, _struct and unions cannot be assigned values directly, nor can be used as function parameters or return type_. Initialization of structure arrays must be fully braced. `double` not supported. Functions are not reentrant unless explicitly declared as such. or `--stack-auto` is specified in `mcs51` port.

- C95: `--std-c95`

- C99: `--std-c99`: compound literals, variable-length arrays, and a lot of long data types are not supported. 

- C11: `--std-c11`: same as above

- C2x: incomplete

### Usage

#### Output

- `src.asm`: assmbler source file

- `src.lst`: assembler listing file

- `src.rst`: listing file updated with linkedit information, created by linkage editor

- `src.sym`: symbol listing

- `src.rel`: object file

- `src.map`: memory map for the load module

- `src.mem`: a file with a summary of memory usage

- `src.ihx`: the load module in Intel hex format

- `src.cdb`: debug information

#### CMD Options

- `-m`: architecture type, defaulted to `mcs51`

- `-I`, `-L`,  `-D`, `-c`, `-E`, `-o`, `-S`, `-nostdinc`, `-nostdlib`: same as in GCC

- `--stack-auto`: All functions in the source file will be compiled as reentrant, i.e. the parameters and local variables will be allocated on the stack.

- `callee-saves func1,func2...`: the compiler by default uses a caller saves convention for register saving across function calls. This option can be used to switch the register saving convention for specified functions.

- `--debug`

- `--cyclomatic`: cyclomatic complexity

- `--xram-loc`: the start location of the external RAM

- `--code-loc`: the start location of the code segment

- `--stack-loc`: the default behavior is placed after the data segment for mcs51

- `--xstack-loc`: by default the external stack is placed after `__pdata` segment

- `--data-loc`: internal RAM data segment

- `--idata-loc`: indirectly addressable internal RAM

... and a lot of other linker options

- `--model-small`/`--model-medium`/`--model-large`/`--model-huge`: for different memory models

### Language Extensions

- intrinsic named address spaces

## SDAS

The output of SDAS assemblers consists of an ascii relocatable object file `.rel`, an assembly listing file `.lst` and a symbol file `.sym`.

A source program is composed of asm statements. A line may contain of 128 characters, longer lines are truncated and lost.

```asm
[label:] operator/directive   operand      [; comments]
```

More than one label may appear within a single label field. Each label so specified is assigned the same address value. 

Reusable symbols are specially formatted symbols used as labels within a block of coding that has been delimited as a reusable symbol block.  Reusable symbols are of the form `n$`, where `n` is a decimal integer from 0 to 65535. Reusable symbols provide a convenient means of generating labels for branch instructions and other  such  references  within reusable symbol blocks. Reusable labels cannot be referenced from outside their respective symbol blocks.  Thus, reusable symbols of the same name can  appear  in other  symbol blocks without conflict.  The range of a reusable symbol block consists of those state- ments between two normally constructed  symbolic  labels.

### Expressions

- double colon `::` defines the label as a global symbol

- `==` global assignment operator defines the symbol as a global symbol

- `=:` defines the symbol as local symbol

- `#`: immediate expression indicator

- `.`current location counter. Absolute program areas may use the `.org` directive to specify the absolute location of the current program counter.

- `<`/`>`: produce the lower/upper byte of the expression

- `0o`/`00`/`oq`/`0Q`: Octal radix operator

#### Directives

- `.byte exp1, exp2, expn`/`.db`/`.fcb`: generate successive bytes of binary data in the object module

- `.word`/`.dw`/`.fdb`: 16-bit

- `.blkb N`/`.ds`/`.rmb`/`.rs`: reserve byte blocks

- `.blkw N`/`.blk3`/`.blk4`: reserve N words (2 bytes)/triples (3 bytes)/quads (4 bytes) of space.

- `.ascii /string/`/`.str`/`.fcc`

- `.ascis /string/`/`.strs`: the last character in the string will have the high order bit set.

- `.asciz /string/`/`.strz`: null character terminated string


- `.radix Radix`: specifies the default radix to be used for succeeding numbers

- `.even`/`.odd`: ensures that the current location counter contains an enen/odd boundary value.

- `.bndry n`: advance the location counter to the next integer multiple of n. This may not be the final address after linking.

- `.area name`: `ABS` (absolute), `REL` (relocatable), `OVR` (overlay), `CON` (concatenate), `NOPAG` (non-paged area), `PAG` (paged area). The .area directive provides a means of defining and separating multiple programming and data sections.   The  name  is  the area  label used by the assembler and the linker to collect code from various separately assembled modules into one section.

- `.org exp`: set current location counter to be the specified absolute value.

- `.globl sym1, sym2, ..., symn`: export symbols 

- `.local sym1, sym2, ..., symn`: define symbols that are local to the current assembly process.

- `.equ`, `.gblequ`, `.lclequ`: `sym = expr`, `sym == exp`, `sym =: exp`

- `.if expr`, `.else`, `.endif`

- `.ifne`/`.ifeq`/`.ifgt`/`.iflt`/`.ifge`/`.ifle`: compared to zero

- `.ifdef`/`.ifndef`

- `.include /string/`

By using macros a programmer can use a single line to insert a sequence of lines into a source program.

```asm
; macro definition
[label:]  .macro  name, dummy argumet list
...
[mexit]
...
.endm
```

; macro calling
[label:] name  real arguments


