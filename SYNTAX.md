# Deimos Syntax Specification
⚠️ means that the feature is marked __unstable__ and likely to change without warning.
The whole language is in flux as it is under active development, but some features more so. 

## Comments
```
-- A single line comment
--- Doc comment
```

## Types
- Nullary: `void, *void`
- Signed integers: `i8, i16, i32, i64`
- Unsigned integers: `u8, u16, u32, u64`
- Floating point: `f32, f64`
- Pointers: `*type`
- Arrays: `[]type`
- User defined structures: `Employee` or `fs::File`

⚠️ Generics are not supported yet. ⚠️

## Import and extern
- `extern` is used to mark a function, or type as externally defined from C. 
- `import` is used to import a file

⚠️ `extern` will be deprecated soon when modules are added, and will be repurposed as a modifier keyword. ⚠️

Consider the below snippet which could be part of a bridge to a C API, like SDL2.
```lua
let SDL_INIT_VIDEO: u32 = 0x00000020
extern function SDL_Init(flags: u32): i32

function main()
    if SDL_Init(SDL_INIT_VIDEO) < 0 then
        ..
    end
end
```

## Expressions
- Literals: `5`, `3.14f`, `3.14`, `"hello world"`
- Array literal: `["cheese", "milk", "eggs"]`
- Array index: `array[1]`, zero-based unlike Lua
- Binary expression: `lhs + rhs`
- Unary expression: `-1`
- Function call: `function_call(arg1, arg2)`

## Statements

### If statement
```lua
if cond then
    block
end

if cond then
    then_block
else
    else_block
end
```

### For and while loop
```lua
for i = 0, 10 do
    block
end

while counter < 10 do
    block
end
```

### Return
```lua
function sum(a:i32,b:i32):i32
    return a + b
end

function quote_armstrong()
    print("That’s one small step for a man, a giant leap for mankind.")

    -- Return without an expression returns void
    return
end
```

## Structs and enums
```lua
struct Employee
    name: string,
    pay: i32,
    job: JobRole
end

-- Enums
-- Enum members are not namespaced for now
enum JobRole
    R_Janitor,
    R_Accountant,
    R_Engineer,
end
```

## Modules
Modules are a way to store related functions and variables (usually constants).

Each file defines a module named after itself, and one module can refer to another module using `::` operator e.g `mathy::square` below.

```lua
-- in mathy.dms
function square(n:i32): i32
    return n * n
end

-- usage from main.dms:
let five_squared: i32 = mathy::square(5)
```