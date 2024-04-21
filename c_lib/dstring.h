#ifndef DEIMOS_STRING_H

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

/* A standard library immutable string that stores it's length */
typedef struct D_String
{
    char *data;
    int length;
} D_String;

D_String string_from_c_str(char *data);
D_String string_from_c_str(char *data)
{
    D_String string;
    string.data = data;
    string.length = 0;

    return string;
}

char *string_to_c_str(D_String string);
char *string_to_c_str(D_String string)
{
    char *data = (char *)malloc(string.length + 1);
    if (data == NULL)
    {
        return NULL;
    }

    memcpy(data, string.data, string.length);
    data[string.length] = '\0';

    return data;
}

#endif // DEIMOS_STRING_H