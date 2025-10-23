#ifndef EE_PARSER_H
#define EE_PARSER_H

#include "ee_lexer.h"

#include "ee_array.h"
#include "ee_string.h"

#define EE_AST_LINKED_BUCKET_START_SIZE    (32)
#define EE_AST_NODE_NULL                   ((Ast_Node_Handle)-1)

static const Token TOKEN_STAB = { 0, .type = -1 };

typedef enum Ast_Type
{
	NODE_LITERAL_EXPR    = 0,  // for example 5
	NODE_BINARY_EXPR     = 1,  // 5 + a
	NODE_FUNC_DEFN       = 2,  // fn name(x: f32, y: f32) -> f32
	NODE_VAR_DECL        = 3,  // x: i32;
	NODE_VAR_DEFN        = 4,  // x := 2; expands to x: i32 = 2;
	NODE_ID_EXPR         = 5,  // x
	NODE_STATEMENT_BLOCK = 6,  // { ... }
	NODE_EOF             = 7
} Ast_Type;

typedef enum Ast_Data_Type
{
	DTYPE_U8, DTYPE_U16, DTYPE_U32, DTYPE_U64,
	DTYPE_S8, DTYPE_S16, DTYPE_S32, DTYPE_S64,
	DTYPE_F32, DTYPE_F64,

	// TODO(eesuck): other types
} Ast_Data_Type;

typedef enum Ast_Bin_Op_Type
{
	BINOP_PLUS = 0,
} Ast_Bin_Op_Type;

typedef size_t Ast_Node_Handle;

typedef struct Ast_Type_Info
{
	Ast_Data_Type dtype;
} Ast_Type_Info;

typedef struct Ast_Lit
{
	Ast_Type_Info type_info;

	union
	{
		u8 as_u8; u16 as_u16; u32 as_u32; u64 as_u64;
		i8 as_i8; i16 as_i16; i32 as_i32; i64 as_i64;
		f32 as_f32; f64 as_f64;
	};
} Ast_Lit;

typedef struct Ast_Id
{
	Ast_Type_Info type_info;
	Str_View name;
	Ast_Node_Handle value;
} Ast_Id;

typedef struct Ast_Binary_Expr
{
	Ast_Bin_Op_Type binop_type;

	Ast_Node_Handle left;
	Ast_Node_Handle right;
} Ast_Binary_Expr;

typedef struct Ast_Func_Defn
{
	Str_View name;
	size_t ins_count;

	Ast_Type_Info    out;
	Ast_Node_Handle param_head;
	Ast_Node_Handle body_head;
} Ast_Func_Defn;

typedef struct Ast_Var_Decl
{
	Ast_Id id;
} Ast_Var_Decl;

typedef struct Ast_Var_Defn
{
	Ast_Id id;
	Ast_Node_Handle val;
} Ast_Var_Defn;

typedef struct Ast_Statement_Block
{
	size_t statements_count;
	Ast_Node_Handle statements_head;
} Ast_Statement_Block;

typedef struct Ast_Node
{
	Ast_Type type;
	Ast_Node_Handle prev;
	Ast_Node_Handle next;

	union
	{
		Ast_Lit as_lit;
		Ast_Id as_id;
		Ast_Binary_Expr as_binop;
		Ast_Func_Defn as_func_defn;
		Ast_Var_Decl as_var_decl;
		Ast_Var_Defn as_var_defn;
		Ast_Statement_Block as_block;
	};
} Ast_Node;

typedef struct Ast_Node_Desc
{
	Ast_Node_Handle handle;
	Ast_Node* node_ptr;
} Ast_Node_Desc;

typedef struct Parser
{
	size_t pos;
	Array nodes;
	Allocator allocator;
	
	const Array* tokens;
} Parser;

Parser ee_pars_new(const Array* tokens, const Allocator* allocator);
Ast_Node_Desc ee_pars_emplace_node(Parser* pars, Ast_Type type, Ast_Node_Handle prev);
Ast_Node_Handle ee_pars_id_decl(Parser* pars, Ast_Node_Handle prev);
void ee_pars_run(Parser* pars);

EE_INLINE void ee_pars_advance(Parser* pars, size_t count)
{
	EE_ASSERT(pars->pos + count <= ee_array_len(pars->tokens), "Trying to advance beyond stream buffer (%zu + %zu >= %zu)", pars->pos, count, ee_array_len(pars->tokens));
	
	pars->pos += count;
}

EE_INLINE const Token* ee_pars_peek(const Parser* pars)
{
	// TODO(eesuck): safety checks

	return (const Token*)ee_array_at(pars->tokens, pars->pos);
}

EE_INLINE const Token* ee_pars_peek_next(const Parser* pars, size_t count)
{
	// TODO(eesuck): safety checks

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
	EE_UNUSED(pars);

	if (ee_pars_peek(pars)->type != pattern)
	{
		return EE_FALSE;
	}

	return EE_TRUE;
}

#endif // EE_PARSER_H