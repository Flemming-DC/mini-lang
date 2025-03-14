#include <stdint.h>

int32_t mylib_return_3()
{
    return 3;
};


/*
-L. → Tells the linker to look for libraries in the current directory 
-le → Links against a library named libe.a or libe.so
---
lib:    gcc -c mylib.c -o mylib.o && ar rcs libmylib.a mylib.o 
app:    gcc main.c -L. -lmylib -o main.exe && main.exe

*/

