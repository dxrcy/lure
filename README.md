# Lure

A simple Lua-inspired scripting language.

Actually it is inspired by Lua, Rust, Zig, Haskell, and probably others.

At the moment is is just a concept...

# Keywords

```
let
func
pub
module
template
end
if
then
else
elseif
for
while
loop
do
in
to ????
break
continue
return
and
or
null ?? nil ?
true
false
use
as  ???
```

# Comments

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

# Types

## Actual types

Returned, as strings, by `std.typeof`.
No further type information is given.

```
null
bool
number
string
table
```

## Type-comment types

Not checked by the interpreter, but may be checked by linters / build tools.

```
# Basic types
null
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
string | null

# Table
{ int }
{ string | int }
{ string | int } | null

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
# I dont think this one is a good idea.
# int | null
?int

# Custom type comments ??
# Idk how this would work
file
```

Generics??? Ideally!!

```
# Similar to Haskell
# Maybe `a` should be explicitely declared
a, a -> a
```

### Type-comment coersion

Given in the form `x -> y`, reading "`x` can be coerced to `y`".

#### Generic subtypes

```
a   -> any
a   -> a | b
b   -> a | b
{a} -> table
```

#### Normal subtypes

```
int        -> number
func (...) -> func
```

#### Equivalent types

```
string <-> {char}
void   <-> never    # Idk about this one
```

