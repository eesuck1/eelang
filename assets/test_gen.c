#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef uint8_t    u8;
typedef uint16_t   u16;
typedef uint32_t   u32;
typedef uint64_t   u64;
typedef int8_t     i8;
typedef int16_t    i16;
typedef int32_t    i32;
typedef int64_t    i64;
typedef float      f32;
typedef double     f64;
typedef long double f80;

i32 GLOBAL_CONST = 100;
i32 test_pointers(i32* ptr)
{
    i32 a = (*(ptr));
    (*(ptr)) = (a + 10);
    return (*(ptr));
}

i32 test_loops_and_scopes(i32 start)
{
    i32 total = start;
    while ((total < 10))
    {
        total = (total + 1);
        if ((total == 5))
