# Deimos Syntax Specification

## Comments
```
-- A single line comment
```

## Types
- Nullary: `void, *void`
- Signed integers: `i8, i16, i32, i64`
- Unsigned integers: `u8, u16, u32, u64`
- Floating point: `f32, f64`
- Pointers: `*type`
- Arrays: `[]type`

Generics are not supported yet.

## Import and extern
- `extern` is used to mark a function, or type as externally defined from C. 
- `import` is used to import a file

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
```

## Modules
```lua
module summer
    function sum_ints(a:i32, b:i32):i32
        return a + b
    end
end
```

Modules are a way to store related functions and **constant** variables.