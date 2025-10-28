#ifndef EE_PARSER_H
#define EE_PARSER_H

#include "ee_lexer.h"

#include "ee_array.h"
#include "ee_string.h"
#include "ee_dict.h"

#define EE_AST_NODE_NULL                   ((Ast_Node_Handle)-1)
#define EE_EXPR_PREC_MIN                   (-1024)
#define EE_EXPR_PREC_MAX                   (1024)

typedef enum Ast_Data_Type
{
	DTYPE_U8      = 0, 
	DTYPE_U16     = 1, 
	DTYPE_U32     = 2, 
	DTYPE_U64     = 3,
	DTYPE_I8      = 4, 
	DTYPE_I16     = 5, 
	DTYPE_I32     = 6, 
	DTYPE_I64     = 7,
	DTYPE_F32     = 8, 
	DTYPE_F64     = 9,
	DTYPE_COUNT,
	// TODO(eesuck): other types
} Ast_Data_Type;

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

typedef enum Ast_Type_Expr_Type
{
	TYPE_FLAT  = 0,
	TYPE_TUPLE = 1,
	TYPE_PTR   = 2,
} Ast_Type_Expr_Type;

typedef enum Ast_Expr_Type
{
	EXPR_LIT       = 0,
	EXPR_BINOP     = 1,
	EXPR_IDENT     = 2,
	EXPR_UNOP      = 3,
	EXPR_FUNC_CALL = 4,
	EXPR_ACCESS    = 5,
	EXPR_INDEX     = 6,
} Ast_Expr_Type;

typedef enum Ast_Stmt_Type
{
	STMT_LET    = 0,
	STMT_BLOCK  = 1,
	STMT_IF     = 2,
	STMT_FOR    = 3,
	STMT_WHILE  = 4,
	STMT_FN     = 5,
	STMT_RETURN = 6,
	STMT_MATCH  = 7,
	STMT_STRUCT = 8,
	STMT_ENUM   = 9,
	STMT_UNION  = 10,
	STMT_ASSIGN = 11,
	STMT_EXPR   = 12,
} Ast_Stmt_Type;

typedef i32 Ast_Precedence;

// Type statics
static const i32 _s_type_size_table[DTYPE_COUNT] = { 1, 2, 4, 8, 1, 2, 4, 8, 4, 8 };
static const i32 _s_type_align_table[DTYPE_COUNT] = { 1, 2, 4, 8, 1, 2, 4, 8, 4, 8 };
static const char* _s_type_name_table[DTYPE_COUNT] = { "u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64" };
static const i32 _s_type_name_len_table[DTYPE_COUNT] = { 2, 3, 3, 3, 2, 3, 3, 3, 3, 3 };

// Binop statics
static const Ast_Precedence _s_op_binop_prec_table[BINOP_COUNT] = { 11, 11, 12, 12, 12, 4, 3, 7, 5, 6, 8, 8, 9, 9, 9, 9, 10, 10, 1, 0 };
static const char* _s_op_binop_name_table[BINOP_COUNT] = { "+", "-", "*", "/", "%", "&&", "||", "&", "|", "^", "==", "!=", "<=", ">=", "<", ">", "<<", ">>", "as", ".." };
static const Token_Type _s_op_binop_token_type_table[BINOP_COUNT] = { '+', '-', '*', '/', '%', TOKEN_AND, TOKEN_OR, '&', '|', '^', TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL, 
	TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL, '<', '>', TOKEN_SHIFT_LEFT, TOKEN_SHIFT_RIGHT, TOKEN_AS, TOKEN_RANGE };
static const i32 _s_op_binop_name_len_table[BINOP_COUNT] = { 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2 };

// Unop statics
static const Ast_Precedence _s_op_unop_prec_table[UNOP_COUNT] = { 13, 13, 13, 13, 13, 13 };
static const char* _s_op_unop_name_table[UNOP_COUNT] = { "!", "*", "&", "~", "+", "-", };
static const i32 _s_op_unop_name_len_table[UNOP_COUNT] = { 1, 1, 1, 1, 1, 1 };
static const Token_Type _s_op_unop_token_type_table[UNOP_COUNT] = { '!', '*', '&', '~', '+', '-' };

static const Str_View _s_str_view_null = { 0 };

typedef size_t Usize;

typedef struct Ast_Expr Ast_Expr;
typedef struct Ast_Stmt Ast_Stmt;
typedef struct Ast_Node Ast_Node;

typedef struct Ast_Func_Type_Info
{
	struct Ast_Type_Info* params;
	struct Ast_Type_Info* ret;
} Ast_Func_Type_Info;

typedef struct Ast_Type_Info
{
	Ast_Type_Expr_Type type;

	union
	{
		const Token* as_flat;
		Array as_tuple;
		struct Ast_Type_Info* as_ptr_to;
		Ast_Func_Type_Info as_func;
	};
} Ast_Type_Info;

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
	Array args;
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

typedef struct Ast_Expr
{
	Ast_Expr_Type type;

	union
	{
		Ast_Lit_Expr as_lit;
		Ast_Ident_Expr as_ident;
		Ast_Binop_Expr as_binop;
		Ast_Unop_Expr as_unop;
		Ast_Func_Call_Expr as_func_call;
		Ast_Access_Expr as_access;
		Ast_Index_Expr as_index;
	};
} Ast_Expr;

typedef struct Ast_Let_Stmt
{
	const Token* ident;
	Ast_Expr* val;
} Ast_Let_Stmt;

typedef struct Ast_Assign_Stmt
{
	Ast_Expr* ident;
	Ast_Expr* val;
} Ast_Assign_Stmt;

typedef struct Ast_Block_Stmt
{
	Array stmts;
} Ast_Block_Stmt;

typedef struct Ast_Expr_Stmt
{
	Ast_Expr* expr;
} Ast_Expr_Stmt;

typedef struct Ast_If_Stmt
{
	Ast_Expr* cond;
	Ast_Block_Stmt* if_block;
	Ast_Block_Stmt* else_block;
} Ast_If_Stmt;

typedef struct Ast_For_Stmt
{
	const Token* it;
	Ast_Expr* range;
	Ast_Block_Stmt* body;
} Ast_For_Stmt;

typedef struct Ast_While_Stmt
{
	Ast_Expr* cond;
	Ast_Block_Stmt* body;
} Ast_While_Stmt;

typedef struct Ast_Func_Decl_Stmt
{
	Array params;

} Ast_Func_Decl_Stmt;

typedef struct Ast_Stmt
{
	Ast_Stmt_Type type;

	union
	{
		Ast_Let_Stmt as_let;
		Ast_Block_Stmt as_block;
		Ast_Assign_Stmt as_assign;
		Ast_Expr_Stmt as_expr;
		Ast_If_Stmt as_if;
		Ast_For_Stmt as_for;
		Ast_While_Stmt as_while;
	};
} Ast_Stmt;

typedef struct Parser
{
	size_t pos;
	Array nodes;
	Dict var_types_table;
	Dict types_table;
	Allocator allocator;
	
	const Array* tokens;
} Parser;

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
	return (token->type == TOKEN_LIT_INT) || (token->type == TOKEN_LIT_FLOAT) || (token->type == TOKEN_LIT_STR);
}

Ast_Binop_Type ee_token_match_binop(const Token* token)
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

Ast_Unop_Type ee_token_match_unop(const Token* token)
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
	EE_ASSERT(pars->pos + count <= ee_array_len(pars->tokens), "Trying to advance beyond stream buffer (%zu + %zu >= %zu)", pars->pos, count, ee_array_len(pars->tokens));
	
	pars->pos += count;
}

EE_INLINE const Token* ee_pars_peek(const Parser* pars)
{
	if (pars->pos >= ee_array_len(pars->tokens))
	{
		return &_s_token_null;
	}

	return (const Token*)ee_array_at(pars->tokens, pars->pos);
}

EE_INLINE const Token* ee_pars_peek_next(const Parser* pars, size_t count)
{
	if (pars->pos + count >= ee_array_len(pars->tokens))
	{
		return &_s_token_null;
	}

	return (const Token*)ee_array_at(pars->tokens, pars->pos + count);
}

EE_INLINE const Token* ee_pars_eat(Parser* pars)
{
	return (const Token*)ee_array_at(pars->tokens, pars->pos++);
}

EE_INLINE i32 ee_pars_match_or_panic(Parser* pars, Token_Type pattern, const char* message)
{
	if (ee_pars_peek(pars)->type != pattern)
	{
		EE_ASSERT(0, "Unexpected token");
		EE_PRINT(message);

		return EE_FALSE;
	}

	ee_pars_advance(pars, 1);
	return EE_TRUE;
}

EE_INLINE i32 ee_pars_check_or_panic(Parser* pars, Token_Type pattern, const char* message)
{
	EE_UNUSED(pars);

	if (ee_pars_peek(pars)->type != pattern)
	{
		EE_ASSERT(0, "Unexpected token");
		EE_PRINT(message);
	
		return EE_FALSE;
	}

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

Ast_Type_Info* ee_pars_type_info(Parser* pars);

Ast_Expr* ee_pars_atom(Parser* pars);
Ast_Expr* ee_pars_postfix(Parser* pars, Ast_Expr* atom);
Ast_Expr* ee_pars_expr_1(Parser* pars, Ast_Expr* lhs, Ast_Precedence min_prec);
Ast_Expr* ee_pars_expr(Parser* pars);

Ast_Stmt* ee_pars_stmt(Parser* pars);

void ee_pars_debug_print_type_info(Ast_Type_Info* root, size_t indent);
void ee_pars_debug_print_expr(Ast_Expr* expr, size_t indent);
void ee_pars_debug_print_stmt(Ast_Stmt* stmt, size_t indent);

Parser ee_pars_new(const Array* tokens, const Allocator* allocator);
void ee_pars_run(Parser* pars);

#endif // EE_PARSER_H