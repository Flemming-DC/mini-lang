
// ---------- PRELUDE ---------- //
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define Crash(message) crash(message, __FILE__, __FUNCTION__, __LINE__)

void crash(const char* message, const char* file, const char* function, int line) {{
    printf("%s in %s at %i: \n", file, function, line );
    printf("ERROR: %s\n", message);
    exit(1);
}}

typedef struct {
    char* ptr; 
    size_t len; 
} char_array;

char* c_str(char_array str) {
    char* c_str = (char*) malloc((str.len + 1) * sizeof(char));
    if (!c_str) 
        Crash("Failed to allocate array.");

    for (size_t i = 0; i < str.len; ++i) {
        c_str[i] = str.ptr[i];
    }
    c_str[str.len] = '\0';
    return c_str;
}

// -------- ARRAY_TYPES -------- //
// ----------- SCRIPT ----------- //
struct Vector2_2 {
    float x;
    float y;
};

struct Vector2_2 add_2 (struct Vector2_2 v, struct Vector2_2 u) 
{
return (struct Vector2_2){.x = v.x + u.x, .y = v.y + u.y};
}
struct LinkList_2 {
    struct LinkList_2* next;
    float data;
};

void foo_2 () 
{
struct LinkList_2 ll0 = (struct LinkList_2){.next = (struct LinkList_2*) NULL, .data = 0.0f};
struct LinkList_2 ll1 = (struct LinkList_2){.next = &ll0, .data = 1.0f};
struct LinkList_2 ll2 = (struct LinkList_2){.next = &ll1, .data = 2.0f};
struct LinkList_2 ll = ll2;
while (ll.next != (struct LinkList_2*) NULL) { {
ll = *ll.next;
} };
}
struct Vector2 {
    float x;
    float y;
};

struct Vector2 add (struct Vector2 v, struct Vector2 u) 
{
return (struct Vector2){.x = v.x + u.x, .y = v.y + u.y};
}


extern int32_t mylib_return_3 ();
extern int32_t mylib_again_return_5 ();
struct LinkList {
    struct LinkList* next;
    float data;
};

void foo () 
{
printf("%d\n", mylib_return_3());;
printf("%d\n", mylib_again_return_5());;
struct LinkList ll0 = (struct LinkList){.next = (struct LinkList*) NULL, .data = 0.0f};
struct LinkList ll1 = (struct LinkList){.next = &ll0, .data = 1.0f};
struct LinkList ll2 = (struct LinkList){.next = &ll1, .data = 2.0f};
struct LinkList ll = ll2;
while (ll.next != (struct LinkList*) NULL) { {
printf("LinkList {");printf("}\n");;
ll = *ll.next;
} };
}

int main(int argc, char *argv[]) {
    foo();
    return 0;
}
        