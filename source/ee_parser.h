#ifndef EE_PARSER_H
#define EE_PARSER_H

#include "ee_lexer.h"
#include "ee_log.h"

#include "ee_array.h"
#include "ee_string.h"
#include "ee_dict.h"

#define EE_AST_NODE_NULL            ((Ast_Node_Handle)-1)
#define EE_EXPR_PREC_MIN            (-1024)
#define EE_EXPR_PREC_MAX            (1024)
#define EE_TYPE_BASE_SIZE           (8)
#define EE_FUNC_ARGS_BASE_SIZE      (8)
#define EE_BLOCK_BASE_STMT_SIZE     (16)
#define EE_MATCH_BASE_CASES_SIZE    (8)
#define EE_MODULE_NAME_MAX_LEN      (256)
#define EE_MODULE_PATH_MAX_LEN      (4096)
#define EE_PARSE_QUEUE_BASE_SIZE    (16)
#define EE_MODULE_EXTENSION_LEN     (3)

typedef enum Ast_Binop_Type
{
	BINOP_PLUS          = 0,
	BINOP_MINUS         = 1,
	BINOP_MUL           = 2,
	BINOP_DIV           = 3,
	BINOP_MOD           = 4,
	BINOP_AND           = 5,
	BINOP_OR            = 6,
	BINOP_BW_AND        = 7,
	BINOP_BW_OR         = 8,
	BINOP_BW_XOR        = 9,
	BINOP_EQUAL         = 10,
	BINOP_NOT_EQUAL     = 11,
	BINOP_LESS_EQUAL    = 12,
	BINOP_GREATER_EQUAL = 13,
	BINOP_LESS          = 14,
	BINOP_GREATER       = 15,
	BINOP_SHIFT_LEFT    = 16,
	BINOP_SHIFT_RIGHT   = 17,
	BINOP_CAST          = 18,
	BINOP_RANGE         = 19,
	BINOP_COUNT,
} Ast_Binop_Type;

typedef enum Ast_Unop_Type
{
	UNOP_NOT    = 0,
	UNOP_DEREF  = 1,
	UNOP_PTR    = 2,
	UNOP_BW_NOT = 3,
	UNOP_PLUS   = 4,
	UNOP_MINUS  = 5,
	UNOP_COUNT,
} Ast_Unop_Type;

typedef enum Ast_Expr_Type
{
	EXPR_LIT         = 0,
	EXPR_BINOP       = 1,
	EXPR_IDENT       = 2,
	EXPR_UNOP        = 3,
	EXPR_FUNC_CALL   = 4,
	EXPR_ACCESS      = 5,
	EXPR_INDEX       = 6,
	EXPR_TYPE_STRUCT = 7,
	EXPR_TYPE_TUPLE  = 8,
	EXPR_TYPE_UNION  = 9,
	EXPR_TYPE_ENUM   = 10,
	EXPR_TYPE_ARRAY  = 11,
	EXPR_INIT_LIST   = 12,
} Ast_Expr_Type;

typedef enum Ast_Stmt_Type
{
	STMT_VAR_DECL  = 0,
	STMT_BLOCK     = 1,
	STMT_IF        = 2,
	STMT_FOR       = 3,
	STMT_WHILE     = 4,
	STMT_FN        = 5,
	STMT_RETURN    = 6,
	STMT_MATCH     = 7,
	STMT_ASSIGN    = 8,
	STMT_EXPR      = 9,
	STMT_BREAK     = 10,
	STMT_CONTINUE  = 11,
	STMT_DEFER     = 12,
	STMT_IMPORT    = 13,
} Ast_Stmt_Type;

typedef enum Ast_Type_Expr_Flag
{
	TYPE_EXPR_CONST = 0,
} Ast_Type_Expr_Flag;

typedef enum Ast_Flag
{
	AST_FLAG_NONE     = 0,
	AST_FLAG_CONST    = (1 << 0),
	AST_FLAG_COMPTIME = (1 << 1),
} Ast_Flag;

typedef i32 Ast_Precedence;
typedef i32 Ast_Custom_Type_Id;
typedef i32 Bool;
typedef size_t Usize;

static const Ast_Precedence _s_op_binop_prec_table[BINOP_COUNT] = {
    [BINOP_PLUS]          = 11,
    [BINOP_MINUS]         = 11,
    [BINOP_MUL]           = 12,
    [BINOP_DIV]           = 12,
    [BINOP_MOD]           = 12,
    [BINOP_AND]           = 4,
    [BINOP_OR]            = 3,
    [BINOP_BW_AND]        = 7,
    [BINOP_BW_OR]         = 5,
    [BINOP_BW_XOR]        = 6,
    [BINOP_EQUAL]         = 8,
    [BINOP_NOT_EQUAL]     = 8,
    [BINOP_LESS_EQUAL]    = 9,
    [BINOP_GREATER_EQUAL] = 9,
    [BINOP_LESS]          = 9,
    [BINOP_GREATER]       = 9,
    [BINOP_SHIFT_LEFT]    = 10,
    [BINOP_SHIFT_RIGHT]   = 10,
    [BINOP_CAST]          = 13,
    [BINOP_RANGE]         = 0,
};

static const char* _s_op_binop_name_table[BINOP_COUNT] = {
    [BINOP_PLUS]          = "+",
    [BINOP_MINUS]         = "-",
    [BINOP_MUL]           = "*",
    [BINOP_DIV]           = "/",
    [BINOP_MOD]           = "%",
    [BINOP_AND]           = "&&",
    [BINOP_OR]            = "||",
    [BINOP_BW_AND]        = "&",
    [BINOP_BW_OR]         = "|",
    [BINOP_BW_XOR]        = "^",
    [BINOP_EQUAL]         = "==",
    [BINOP_NOT_EQUAL]     = "!=",
    [BINOP_LESS_EQUAL]    = "<=",
    [BINOP_GREATER_EQUAL] = ">=",
    [BINOP_LESS]          = "<",
    [BINOP_GREATER]       = ">",
    [BINOP_SHIFT_LEFT]    = "<<",
    [BINOP_SHIFT_RIGHT]   = ">>",
    [BINOP_CAST]          = "as",
    [BINOP_RANGE]         = "..",
};

static const Token_Type _s_op_binop_token_type_table[BINOP_COUNT] = {
    [BINOP_PLUS]          = '+',
    [BINOP_MINUS]         = '-',
    [BINOP_MUL]           = '*',
    [BINOP_DIV]           = '/',
    [BINOP_MOD]           = '%',
    [BINOP_AND]           = TOKEN_AND,
    [BINOP_OR]            = TOKEN_OR,
    [BINOP_BW_AND]        = '&',
    [BINOP_BW_OR]         = '|',
    [BINOP_BW_XOR]        = '^',
    [BINOP_EQUAL]         = TOKEN_EQUAL_EQUAL,
    [BINOP_NOT_EQUAL]     = TOKEN_NOT_EQUAL,
    [BINOP_LESS_EQUAL]    = TOKEN_LESS_EQUAL,
    [BINOP_GREATER_EQUAL] = TOKEN_GREATER_EQUAL,
    [BINOP_LESS]          = '<',
    [BINOP_GREATER]       = '>',
    [BINOP_SHIFT_LEFT]    = TOKEN_SHIFT_LEFT,
    [BINOP_SHIFT_RIGHT]   = TOKEN_SHIFT_RIGHT,
    [BINOP_CAST]          = TOKEN_AS,
    [BINOP_RANGE]         = TOKEN_RANGE,
};

static const i32 _s_op_binop_name_len_table[BINOP_COUNT] = {
    [BINOP_PLUS]          = 1,
    [BINOP_MINUS]         = 1,
    [BINOP_MUL]           = 1,
    [BINOP_DIV]           = 1,
    [BINOP_MOD]           = 1,
    [BINOP_AND]           = 2,
    [BINOP_OR]            = 2,
    [BINOP_BW_AND]        = 1,
    [BINOP_BW_OR]         = 1,
    [BINOP_BW_XOR]        = 1,
    [BINOP_EQUAL]         = 2,
    [BINOP_NOT_EQUAL]     = 2,
    [BINOP_LESS_EQUAL]    = 2,
    [BINOP_GREATER_EQUAL] = 2,
    [BINOP_LESS]          = 1,
    [BINOP_GREATER]       = 1,
    [BINOP_SHIFT_LEFT]    = 2,
    [BINOP_SHIFT_RIGHT]   = 2,
    [BINOP_CAST]          = 2,
    [BINOP_RANGE]         = 2,
};

static const Ast_Precedence _s_op_unop_prec_table[UNOP_COUNT] = {
    [UNOP_NOT]    = 13,
    [UNOP_DEREF]  = 13,
    [UNOP_PTR]    = 13,
    [UNOP_BW_NOT] = 13,
    [UNOP_PLUS]   = 13,
    [UNOP_MINUS]  = 13,
};

static const char* _s_op_unop_name_table[UNOP_COUNT] = {
    [UNOP_NOT]    = "!",
    [UNOP_DEREF]  = "*",
    [UNOP_PTR]    = "&",
    [UNOP_BW_NOT] = "~",
    [UNOP_PLUS]   = "+",
    [UNOP_MINUS]  = "-",
};

static const i32 _s_op_unop_name_len_table[UNOP_COUNT] = {
    [UNOP_NOT]    = 1,
    [UNOP_DEREF]  = 1,
    [UNOP_PTR]    = 1,
    [UNOP_BW_NOT] = 1,
    [UNOP_PLUS]   = 1,
    [UNOP_MINUS]  = 1,
};

static const Token_Type _s_op_unop_token_type_table[UNOP_COUNT] = {
    [UNOP_NOT]    = '!',
    [UNOP_DEREF]  = '*',
    [UNOP_PTR]    = '&',
    [UNOP_BW_NOT] = '~',
    [UNOP_PLUS]   = '+',
    [UNOP_MINUS]  = '-',
};
typedef struct Ast_Expr Ast_Expr;
typedef struct Ast_Stmt Ast_Stmt;

// TODO(eesuck): remove type info from var and just store it globaly
typedef struct Ast_Lit_Expr
{
	const Token* token;
} Ast_Lit_Expr;

typedef struct Ast_Ident_Expr
{
	const Token* token;
} Ast_Ident_Expr;

typedef struct Ast_Binop_Expr
{
	Ast_Binop_Type type;
	Ast_Expr* left;
	Ast_Expr* right;
} Ast_Binop_Expr;

typedef struct Ast_Unop_Expr
{
	Ast_Unop_Type type;
	Ast_Expr* expr;
} Ast_Unop_Expr;

typedef struct Ast_Func_Call_Expr
{
	Linked_Array args;  // Ast_Expr*
	Ast_Expr* func;
} Ast_Func_Call_Expr;

typedef struct Ast_Access_Expr
{
	Ast_Expr* entity;
	const Token* member;
} Ast_Access_Expr;

typedef struct Ast_Index_Expr
{
	Ast_Expr* entity;
	Ast_Expr* index;
} Ast_Index_Expr;

typedef struct Ast_Type_Expr_Struct
{
	Linked_Array members; // const Token*
	Linked_Array types;   // Ast_Expr*
} Ast_Type_Expr_Struct;

typedef struct Ast_Type_Expr_Tuple
{
	Linked_Array types;   // Ast_Expr*
} Ast_Type_Expr_Tuple;

typedef struct Ast_Type_Expr_Union
{
	Linked_Array members; // const Token*
	Linked_Array types;   // Ast_Expr*
} Ast_Type_Expr_Union;

typedef struct Ast_Type_Expr_Enum
{
	Linked_Array members; // const Token*
	Linked_Array values;  // Ast_Expr*
} Ast_Type_Expr_Enum;

typedef struct Ast_Type_Expr_Array
{
	Ast_Expr* size;
	Ast_Expr* type_expr;
} Ast_Type_Expr_Array;

typedef struct Ast_Init_List
{
	Linked_Array members; // const Token*
	Linked_Array values;  // Ast_Expr*
} Ast_Init_List;

typedef struct Ast_Expr
{
	Ast_Expr_Type type;
	i32 flags;

	union
	{
		Ast_Lit_Expr as_lit;
		Ast_Ident_Expr as_ident;
		Ast_Binop_Expr as_binop;
		Ast_Unop_Expr as_unop;
		Ast_Func_Call_Expr as_func_call;
		Ast_Access_Expr as_access;
		Ast_Index_Expr as_index;
		Ast_Type_Expr_Struct as_type_struct;
		Ast_Type_Expr_Tuple as_type_tuple;
		Ast_Type_Expr_Union as_type_union;
		Ast_Type_Expr_Array as_type_array;
		Ast_Type_Expr_Enum as_type_enum;
		Ast_Init_List as_init_list;
	};
} Ast_Expr;

typedef struct Ast_Var_Decl_Stmt
{
	const Token* ident;
	Ast_Expr* val;
	Ast_Expr* type_expr;
} Ast_Var_Decl_Stmt;

typedef struct Ast_Assign_Stmt
{
	Ast_Expr* ident;
	Ast_Expr* val;
} Ast_Assign_Stmt;

typedef struct Ast_Block_Stmt
{
	Linked_Array stmts; // Ast_Stmt*
} Ast_Block_Stmt;

typedef struct Ast_Expr_Stmt
{
	Ast_Expr* expr;
} Ast_Expr_Stmt;

typedef struct Ast_If_Stmt
{
	Ast_Expr* cond;
	Ast_Stmt* if_block;
	Ast_Stmt* else_block;
} Ast_If_Stmt;

typedef struct Ast_For_Stmt
{
	const Token* it;
	Ast_Expr* range;
	Ast_Stmt* body;
} Ast_For_Stmt;

typedef struct Ast_While_Stmt
{
	Ast_Expr* cond;
	Ast_Stmt* body;
} Ast_While_Stmt;

typedef struct Ast_Func_Param
{
	const Token* ident;
	Ast_Expr* type;
} Ast_Func_Param;

typedef struct Ast_Func_Decl_Stmt
{
	const Token* ident;
	Linked_Array params;
	Ast_Expr* ret_type;
	Ast_Stmt* body;
} Ast_Func_Decl_Stmt;

typedef struct Ast_Ret_Stmt
{
	Ast_Expr* val;
} Ast_Ret_Stmt;

typedef struct Ast_Defer_Stmt
{
	Ast_Stmt* stmt;
} Ast_Defer_Stmt;

typedef struct Ast_Match_Stmt
{
	Ast_Expr* expr;
	Ast_Stmt* def_stmt;
	Linked_Array cases;
	Linked_Array stmts;
} Ast_Match_Stmt;

typedef struct Ast_Import_Stmt
{
	const Token* token;
} Ast_Import_Stmt;

typedef struct Ast_Stmt
{
	Ast_Stmt_Type type;
	i32 flags;

	union
	{
		Ast_Var_Decl_Stmt as_var_decl;
		Ast_Block_Stmt as_block;
		Ast_Assign_Stmt as_assign;
		Ast_Expr_Stmt as_expr;
		Ast_If_Stmt as_if;
		Ast_For_Stmt as_for;
		Ast_While_Stmt as_while;
		Ast_Func_Decl_Stmt as_func_decl;
		Ast_Ret_Stmt as_ret;
		Ast_Defer_Stmt as_defer;
		Ast_Match_Stmt as_match;
		Ast_Import_Stmt as_import;
	};
} Ast_Stmt;

typedef struct Ast_Module
{
	char name[EE_MODULE_NAME_MAX_LEN];
	Ast_Stmt* root;
	Linked_Array imports;
} Ast_Module;

typedef struct Parser
{
	size_t pos;
	Logger logger;
	Lexer lex;
	Linked_Array imports;
	Allocator allocator;
} Parser;

typedef struct Parse_Project
{
	Dict modules_map;
	Linked_Array modules;
	Allocator allocator;
} Parse_Project;

Ast_Expr* ee_alloc_expr(Parser* pars, Ast_Expr_Type type);
Ast_Stmt* ee_alloc_stmt(Parser* pars, Ast_Stmt_Type type);

Ast_Expr* ee_pars_primary(Parser* pars);
Ast_Expr* ee_pars_atom(Parser* pars);
Ast_Expr* ee_pars_postfix(Parser* pars, Ast_Expr* atom);
Ast_Expr* ee_pars_expr_1(Parser* pars, Ast_Expr* lhs, Ast_Precedence min_prec);
Ast_Expr* ee_pars_expr(Parser* pars);

Ast_Stmt* ee_pars_stmt(Parser* pars);

void ee_pars_debug_print_expr(Ast_Expr* expr, size_t indent);
void ee_pars_debug_print_stmt(Ast_Stmt* stmt, size_t indent);
void ee_pars_debug_print_module(Ast_Module* mod);
void ee_pars_debug_print_project(Parse_Project* proj);


Ast_Module* ee_pars_file(const char* file_path, const Allocator* allocator);
Parse_Project ee_pars_queue_run(const char* root_path, const Allocator* allocator);

EE_INLINE void ee_stmt_set_flag(Ast_Stmt* stmt, Ast_Flag flag)
{
	stmt->flags |= flag;
}

EE_INLINE void ee_stmt_remove_flag(Ast_Stmt* stmt, Ast_Flag flag)
{
	stmt->flags &= ~flag;
}

EE_INLINE Bool ee_stmt_has_flag(Ast_Stmt* stmt, Ast_Flag flag)
{
	return stmt->flags & flag;
}

EE_INLINE void ee_expr_set_flag(Ast_Expr* expr, Ast_Flag flag)
{
	expr->flags |= flag;
}

EE_INLINE void ee_expr_remove_flag(Ast_Expr* expr, Ast_Flag flag)
{
	expr->flags &= ~flag;
}

EE_INLINE Bool ee_expr_has_flag(Ast_Expr* expr, Ast_Flag flag)
{
	return expr->flags & flag;
}

EE_INLINE i32 ee_op_prec(Ast_Binop_Type op)
{
	if (op == BINOP_COUNT)
	{
		return EE_EXPR_PREC_MIN - 1;
	}

	return _s_op_binop_prec_table[op];
}

EE_INLINE i32 ee_token_is_lit(const Token* token)
{
	return (token->type == TOKEN_LIT_INT) || (token->type == TOKEN_LIT_FLOAT) || (token->type == TOKEN_LIT_STR) ||
		(token->type == TOKEN_TRUE) || (token->type == TOKEN_FALSE);
}

EE_INLINE Ast_Binop_Type ee_token_match_binop(const Token* token)
{
	if (token->type == TOKEN_INVALID)
	{
		return BINOP_COUNT;
	}

	for (Ast_Binop_Type binop = 0; binop < BINOP_COUNT; ++binop)
	{
		if (token->type == _s_op_binop_token_type_table[binop])
		{
			return binop;
		}
	}

	return BINOP_COUNT;
}

EE_INLINE Ast_Unop_Type ee_token_match_unop(const Token* token)
{
	if (token->type == TOKEN_INVALID)
	{
		return UNOP_COUNT;
	}

	for (Ast_Unop_Type unop = 0; unop < UNOP_COUNT; ++unop)
	{
		if (token->type == _s_op_unop_token_type_table[unop])
		{
			return unop;
		}
	}

	return UNOP_COUNT;
}

EE_INLINE void ee_pars_advance(Parser* pars, size_t count)
{
	EE_ASSERT(pars->pos + count <= ee_array_len(&pars->lex.tokens), "Trying to advance beyond stream buffer (%zu + %zu >= %zu)", pars->pos, count, ee_array_len(&pars->lex.tokens));

	pars->pos += count;
}

EE_INLINE const Token* ee_pars_peek(const Parser* pars)
{
	if (pars->pos >= ee_array_len(&pars->lex.tokens))
	{
		return &_s_token_null;
	}

	return (const Token*)ee_array_at(&pars->lex.tokens, pars->pos);
}

EE_INLINE const Token* ee_pars_peek_next(const Parser* pars, size_t count)
{
	if (pars->pos + count >= ee_array_len(&pars->lex.tokens))
	{
		return &_s_token_null;
	}

	return (const Token*)ee_array_at(&pars->lex.tokens, pars->pos + count);
}

EE_INLINE const Token* ee_pars_eat(Parser* pars)
{
	return (const Token*)ee_array_at(&pars->lex.tokens, pars->pos++);
}

EE_INLINE i32 ee_pars_match_or_panic(Parser* pars, Token_Type pattern, const char* message, ...)
{
	va_list args;
	va_start(args, message);

	if (ee_pars_peek(pars)->type != pattern)
	{
		ee_log_error_token_va(&pars->logger, ee_pars_peek(pars), message, args);
		EE_ASSERT(0, "");

		return EE_FALSE;
	}

	ee_pars_advance(pars, 1);
	va_end(args);

	return EE_TRUE;
}

EE_INLINE i32 ee_pars_check_or_panic(Parser* pars, Token_Type pattern, const char* message, ...)
{
	va_list args;
	va_start(args, message);

	if (ee_pars_peek(pars)->type != pattern)
	{
		ee_log_error_token_va(&pars->logger, ee_pars_peek(pars), message, args);
		EE_ASSERT(0, "");

		return EE_FALSE;
	}

	va_end(args);

	return EE_TRUE;
}

EE_INLINE i32 ee_pars_match(Parser* pars, Token_Type pattern)
{
	if (ee_pars_peek(pars)->type != pattern)
	{
		return EE_FALSE;
	}

	ee_pars_advance(pars, 1);
	return EE_TRUE;
}

EE_INLINE i32 ee_pars_check(Parser* pars, Token_Type pattern)
{
	if (ee_pars_peek(pars)->type != pattern)
	{
		return EE_FALSE;
	}

	return EE_TRUE;
}

EE_INLINE void ee_print_indent(size_t indent)
{
	for (size_t i = 0; i < indent; ++i)
	{
		EE_PRINT("  ");
	}
}

EE_INLINE void ee_print_with_indent(size_t indent, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	ee_print_indent(indent);
	vprintf(fmt, args);

	va_end(args);
}

EE_INLINE void ee_println_with_indent(size_t indent, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	ee_print_indent(indent);
	vprintf(fmt, args);
	putc('\n', stdout);

	va_end(args);
}

EE_INLINE i32 ee_streq(const u8* a, const u8* b, size_t len)
{
	EE_UNUSED(len);

	return strcmp((const char*)a, (const char*)b) == 0;
}

EE_INLINE i32 ee_token_eq(const u8* a, const u8* b, size_t len)
{
	EE_UNUSED(len);

	const Token* tok_a = *(const Token**)a;
	const Token* tok_b = *(const Token**)b;

	if (tok_a->scratch.len != tok_b->scratch.len)
	{
		return EE_FALSE;
	}

	return memcmp(tok_a->scratch.buffer, tok_b->scratch.buffer, tok_a->scratch.len) == 0;
}

EE_INLINE u64 ee_token_hash(const u8* a, size_t len)
{
	EE_UNUSED(len);

	const Token* tok_a = *(const Token**)a;

	return ee_hash_fast(tok_a->scratch.buffer, tok_a->scratch.len);
}

#endif // EE_PARSER_H