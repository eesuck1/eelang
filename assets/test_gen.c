#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef uint8_t     u8;
typedef uint16_t    u16;
typedef uint32_t    u32;
typedef uint64_t    u64;
typedef int8_t      i8;
typedef int16_t     i16;
typedef int32_t     i32;
typedef int64_t     i64;
typedef float       f32;
typedef double      f64;
typedef long double f80;

i32 add(i32 a, i32 b)
{
return (a+b);
}

i32 main()
{
i32 a = (-100);
i32 b = 2;
i32 c = ((add)(a, b));
i32* d = (&c);
c = (a+b);
return 0;
}

