#ifndef DEIMOS_PRELUDE_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef char* cstring;

/* Our standard library imports */

/* A standard library immutable string that stores it's length */
// #include "dstring.h"

void printc(char* string);
void printc(char* string)
{
    printf("%s\n", string);
}

#endif // DEIMOS_PRELUDE_H