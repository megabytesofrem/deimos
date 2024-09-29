
# Table of Contents

1.  [Roadmap](#orgc3f9907)
    1.  [Current](#org54ae489)
    2.  [Future Goals](#org5b5683b)



<a id="orgc3f9907"></a>

# Roadmap

Currently Deimos is very much a work-in-progress.

This document is intended to keep track of breaking changes (and there will be **many**) in the ABI as well as the overall status of the language.


<a id="org54ae489"></a>

## Current

-   [X] Lexical analysis
-   [-] Parsing
    -   [X] Comments
    -   [X] Variables e.g `let foo: i32 = 123`
    -   [X] Expressions
    -   [X] Function declarations
    -   [X] Struct/enum declarations
    -   [X] If statements
    -   [X] For loops
    -   [ ] Modules

-   [X] Name resolution
    -   [X] Scoped name resolution
    -   [X] Module wide name resolution

-   [-] Typechecking (mostly)


<a id="org5b5683b"></a>

## Future Goals

These are things that I would like to accomplish with the language in the future

-   [-] Robust module system (half done)
-   [ ] Generics (the framework is in place)
-   [ ] Option and Result type like Rust/Haskell?
-   [ ] Inline Assembly syntax
-   [ ] Some sort of standard library
-   [ ] Codegen to C

