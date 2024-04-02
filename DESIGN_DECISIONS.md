## AST transformations
1. AST created from the parsed input
2. AST is annotated with types and a HIR is created
3. Control flow analysis

## Type system
- Variables are statically typed and checked at compile-time and specified like `local x:int = 1`.
- The valid types are: `int,float,string,bool,void` as well as arrays and pointer (`[]int` and `*int`)

## Function scoping
- Functions are scoped globally unlike in Lua so there is no such thing as a "local function".

## C interoperability
- Ints and floats map to C `int32_t` and `float` type
- Pointer types map to C pointer types
- Most of the standard library will be written in C and just have thin wrappers

## Import Resolution
- Each file is its own module implicitly named the name of the file e.g `foo.ds` is module `foo`.
- A module can specify a list of imports which it can use