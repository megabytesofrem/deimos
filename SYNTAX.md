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

⚠️ `extern` will be deprecated soon when modules are added, and will be repurposed as a modifier keyword.

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
Modules are a way to store related functions and variables (usually constants). 
Modules are desugared at compile-time and are the future replacement for the `extern` keyword, due
to them supporting a limited form of namespacing (as of current, modules cannot contain nested modules).

```lua
module mathy
    let PI: f32 = 3.14159

    function sum(a:i32, b:i32):i32
        return a + b
    end

    -- csquare is defined externally in C, called mathy_csquare (read above for why)
    extern function csquare(n:i32): i322    
end

-- Usage:
let value_of_pi: f32 = mathy::PI
let sum: i32 = mathy::sum(1, 2)
```