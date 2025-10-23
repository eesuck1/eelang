#ifndef EE_LEXER_H
#define EE_LEXER_H

#include "ctype.h"

#include "ee_arena.h"
#include "ee_array.h"
#include "ee_string.h"

#define EE_ALPHA_BIT_MASK      (~(1 << 5))
#define EE_LEXER_DEF_REWIND    (64)
#define EE_MAX(a, b)           ((a) > (b) ? (a) : (b))

#define EE_LEX_FN_STR          ("fn")

EE_INLINE i32 ee_is_ident_start(char c)
{
    return (c >= 'A' && c <= 'Z') ||
           (c >= 'a' && c <= 'z') ||
           (c == '_');
}

EE_INLINE i32 ee_is_ident_continue(char c)
{
    return (c >= 'A' && c <= 'Z') ||
           (c >= 'a' && c <= 'z') ||
           (c >= '0' && c <= '9') ||
           (c == '_');
}

EE_INLINE i32 ee_is_alpha(char c)
{
    char m_c = c & EE_ALPHA_BIT_MASK;

    return m_c >= 'A' && m_c <= 'Z';
}

EE_INLINE i32 ee_is_digit(char c)
{
    return (c >= '0' && c <= '9');
}

typedef enum Token_Type
{
    // NOTE(eesuck): all single char token used directly
    
    TOKEN_IDENTIFIER  = 256,
    TOKEN_FUNCTION    = 257,
    TOKEN_LIT_INT     = 258,
    TOKEN_LIT_FLOAT   = 259,
    TOKEN_LIT_STR     = 260,
    TOKEN_EOF         = 261,
    TOKEN_COLON_EQUAL = 262,
} Token_Type;

typedef struct Token
{
    Token_Type type;

    union
    {
        f64 as_f64;
        u64 as_u64;
        Str_View as_str_view;
    };

    Str_View scratch;
} Token;

typedef struct Lexer
{
    Arena tokens_parsed;
    Array tokens;

    Str stream;
    size_t pos;
    const char* file_path;
    
    Allocator allocator;
} Lexer;

u64 ee_lex_parse_int(Str_View raw);

Lexer ee_lex_new_file(const char* file_path, const Allocator* allocator);
i32  ee_lex_skip_whitespace(Lexer* lex);
i32  ee_lex_process_id(Lexer* lex);
i32  ee_lex_process_literal(Lexer* lex);
void ee_lex_emit_token(Lexer* lex, Token_Type token_type, Str_View scratch);
void ee_lex_emit_token_u64(Lexer* lex, Str_View scratch, u64 val);
void ee_lex_emit_token_f64(Lexer* lex, Str_View scratch, f64 val);
void ee_lex_emit_token_str_view(Lexer* lex, Token_Type token_type, Str_View scratch, Str_View val);
void ee_lex_tokenize(Lexer* lex);

EE_INLINE i32 ee_lex_end(const Lexer* lex)
{
    return lex->pos >= lex->stream.top;
}

EE_INLINE char ee_lex_peek(const Lexer* lex)
{
    return lex->stream.buffer[lex->pos];
}

EE_INLINE char ee_lex_peek_next(const Lexer* lex, size_t count)
{
    if (lex->pos + count >= lex->stream.top)
    {
        return -1;
    }

    return lex->stream.buffer[lex->pos + count];
}

EE_INLINE char ee_lex_eat(Lexer* lex)
{
    if (lex->pos >= lex->stream.top)
    {
        return -1;
    }

    return lex->stream.buffer[lex->pos++];
}

EE_INLINE void ee_lex_advance(Lexer* lex, size_t count)
{
    EE_ASSERT(lex->pos + count <= lex->stream.top, "Trying to advance beyond stream buffer (%zu + %zu >= %zu)", lex->pos, count, lex->stream.top);
    
    lex->pos += count;
}

EE_INLINE i32 ee_lex_check(const Lexer* lex, char pattern)
{
    return ee_lex_peek(lex) == pattern;
}

EE_INLINE i32 ee_lex_check_many(const Lexer* lex, const char* pattern)
{
    size_t start = lex->pos;

    while (start < lex->stream.top && *pattern != '\0' && lex->stream.buffer[start] == *pattern)
    {
        start++;
        pattern++;
    }

    return *pattern == '\0';
}

#endif // EE_LEXER_H