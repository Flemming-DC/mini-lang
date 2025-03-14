#include <stdint.h>

int32_t mylib_again_return_5()
{
    return 5;
};


/*
-L. → Tells the linker to look for libraries in the current directory 
-le → Links against a library named libe.a or libe.so
---
lib:    gcc -c mylib_again.c -o mylib_again.o && ar rcs libmylib_again.a mylib_again.o 
app:    gcc main.c -L. -lmylib_again -o main.exe && main.exe

*/

