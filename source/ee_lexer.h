#ifndef EE_LEXER_H
#define EE_LEXER_H

#include "ctype.h"

#include "ee_arena.h"
#include "ee_array.h"
#include "ee_string.h"

#define EE_ALPHA_BIT_MASK      (~(1 << 5))
#define EE_LEXER_DEF_REWIND    (64)
#define EE_MAX(a, b)           ((a) > (b) ? (a) : (b))

EE_INLINE i32 ee_is_alpha(char c)
{
    char m_c = c & EE_ALPHA_BIT_MASK;

    return m_c >= 'A' && m_c <= 'Z';
}

EE_INLINE i32 ee_is_ident_start(char c)
{
    return ee_is_alpha(c) || (c == '_');
}

EE_INLINE i32 ee_is_digit(char c)
{
    return (c >= '0' && c <= '9');
}

EE_INLINE i32 ee_is_hex_digit(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

EE_INLINE i32 ee_is_ident_continue(char c)
{
    return ee_is_alpha(c) || ee_is_digit(c) || (c == '_');
}

typedef enum Token_Type
{
    // NOTE(eesuck): all single char token used directly
    
    TOKEN_IDENTIFIER    = 256,
    TOKEN_FUNCTION      = 257,
    TOKEN_LIT_INT       = 258,
    TOKEN_LIT_FLOAT     = 259,
    TOKEN_LIT_STR       = 260,
    TOKEN_EOF           = 261,
    TOKEN_PLUS_EQUAL    = 262,
    TOKEN_MINUS_EQUAL   = 263,
    TOKEN_DIV_EQUAL     = 264,
    TOKEN_MUL_EQUAL     = 265,
    TOKEN_OR_EQUAL      = 266,
    TOKEN_AND_EQUAL     = 267,
    TOKEN_EQUAL_EQUAL   = 268,
    TOKEN_NOT_EQUAL     = 269,
    TOKEN_BW_NOT_EQUAL  = 270,
    TOKEN_BW_AND_EQAUL  = 271,
    TOKEN_BW_OR_EQUAL   = 272,
    TOKEN_BW_XOR_EQUAL  = 273,
    TOKEN_LESS_EQUAL    = 274,
    TOKEN_GREATER_EQUAL = 275,
    TOKEN_MOD_EQUAL     = 276,
    TOKEN_SHIFT_LEFT    = 277,
    TOKEN_SHIFT_RIGHT   = 278,
    TOKEN_AND           = 279,
    TOKEN_OR            = 280,
    TOKEN_LET           = 281,
    TOKEN_AS            = 282,
    TOKEN_IF            = 283,
    TOKEN_FOR           = 284,
    TOKEN_WHILE         = 285,
    TOKEN_CONST         = 286,
    TOKEN_BREAK         = 287,
    TOKEN_CONTINUE      = 288,
    TOKEN_ELSE          = 289,
    TOKEN_NULL          = 290,
    TOKEN_RETURN        = 291,
    TOKEN_STRUCT        = 292,
    TOKEN_UNION         = 293,
    TOKEN_ENUM          = 294,
    TOKEN_MATCH         = 295,
    TOKEN_CASE          = 296,
    TOKEN_TRUE          = 297,
    TOKEN_FALSE         = 298,
    TOKEN_RANGE         = 299,
    TOKEN_IN            = 300,

    TOKEN_INVALID       = -1024
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
    Array tokens;

    Str stream;
    size_t pos;
    const char* file_path;
    
    Allocator allocator;
} Lexer;

static const Token _s_token_null = { .type = TOKEN_INVALID };

u64 ee_lex_parse_int(Str_View raw);
f64 ee_lex_parse_float(Str_View raw);
void ee_lex_debug_print_lit_val(const Token* token);

Lexer ee_lex_new_file(const char* file_path, Allocator* allocator);
i32 ee_lex_skip_whitespace(Lexer* lex);
i32 ee_lex_process_id(Lexer* lex);
i32 ee_lex_process_literal(Lexer* lex);
void ee_lex_emit_after_equal(Lexer* lex, Token_Type token, Token_Type if_equal);
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

EE_INLINE i32 ee_lex_match(const Lexer* lex, char pattern)
{
    if (ee_lex_peek(lex) == pattern)
    {
        ee_lex_advance(lex, 1);
        return EE_TRUE;
    }

    return EE_FALSE;
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