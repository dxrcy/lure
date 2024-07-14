# Lure

A simple Lua-inspired scripting language.

Actually it is inspired by Lua, Rust, Zig, Haskell, Go, and probably others.

At the moment is is just a concept...

# Parsing TODO

nothing.

# Syntax

## Keywords

```
let
func
module
template
end
if
then
else
elif
for
while
do
in
to        # ????
break
continue
return
and
or
as        # ??? 
self      # Maybe it wont be a keyword ??
nil
true
false
```

## Operators / Special Characters

(see keywords for `not`, etc)

```
( )
{ }
[ ]
,

"
'

+
-
*
/
%

&

=

==
/=
>
<
>=
<=

_
..

#
##
#!
#:
```

In type-comments:

```
,
|
->
```

Only semantic meaning:

```
@
```

## Comments

```lure
# Normal comment
#[[
    Block comment
    Can be inline or multiline
]]

# Documentation comment
## A special value
let x = 420

# Module-level documentation comment
# Must be at top of module/file
#! Some module information

# If followed immediately by a non-whitespace character, it is treated as a
# shebang:
#!/bin/lure

# Type comment
# Optional, but recommended
#: int
let x = 42
#: int -> int
func add_one(x)
    return x + 1
end
```

# Print functions

Similar print/write functions to Rust, with the addition of `log` for debugging.

`write` and `writeln`

Write formatted string to file.

`print` and `println`

Print formatted string to stdout.

`log`

Print each argument to stderr. Separate with space, and end with newline.
Each argument is converted to a string automatically.

# Types

## Actual types

Returned, as strings, by `std.type_of`.
No further type information is given.

Maybe change `number` to `int` and `float` ?

```
nil
bool
number (f64)
char    # Maybe this is a bad idea?
string
table
func
```

Tables are passed by reference always.

Functions can interact with variables in their parent scope.

## Type-comment types

Not checked by the interpreter, but may be checked by linters / build tools.

```
# Basic types
nil
bool
int
number (real number / float)
string
char
table

# Special types
# Any type, it can be anything
any
# Returns no value. Like `()` in rust
void
# Will never return (program exit or infinite loop)
never

# Union types
string | int
string | nil

# Table
# Just values (positional)
{ bool }
# Keys and values (named)
{ string:bool }
# Unions
{ string | bool }
{ string:string | string:bool }
# Use explicit key type (int) for mixed positional/named items
{ int:string | string:bool }
# Combining unions and tables
{ int:bool } | { string:int }
{ string | int } | nil
# Heterogenous positional tables
{ string, bool }

# Results as tables
{ err = string } | { ok = int }
# Note that { <err>, <ok> } is used in `core`

# Functions
int, int -> int
int -> void
void -> int

# Closures / callbacks / higher order functions
# Any function, regardless of signature
func
# Same syntax as function signature, but with `func ( ... )`
func (int, int -> int)
func (void -> void)
```

Maybe at some point these will be added for nicer syntax:

```
# Custom types in type-comments ??
# Idk how this would work
file

# Result syntax
# This would be nice
string!int

# Nil syntax
# This is not as necessary
?string
```

Generics??? Ideally!!

```
# Similar to Haskell
a -> a
# Maybe `a` should be explicitely declared
[a] a -> a
a :: a -> a
a -> a :: a
```

### Type-comment coersion

Given in the form `x => y`, reading "`x` can be coerced to `y`".

#### Generic subtypes

```
a   => any
a   => a | b
b   => a | b
{a} => table
```

#### Normal subtypes

```
int        => number
func (...) => func
char       => string
```

#### Equivalent types

```
string <=> {char}
void   <=> never    # Idk about this one
```

