#ifndef EE_SEMANTIC_H
#define EE_SEMANTIC_H

#include "ee_parser.h"
#include "ee_dict.h"

EE_INLINE u64 ee_token_ptr_hash(const u8* key, size_t len)
{
	EE_UNUSED(len);
	const Token* token = *(const Token**)key;

	EE_ASSERT(token->type == TOKEN_IDENTIFIER, "Wrong key for type table hash function, expected identifier, got (%d)", token->type);

	return ee_hash_fast((const u8*)token->scratch.buffer, token->scratch.len);
}

EE_INLINE i32 ee_token_ptr_eq(const u8* a, const u8* b, size_t len)
{
	EE_UNUSED(len);

	const Token* a_token = *(const Token**)a;
	const Token* b_token = *(const Token**)b;

	EE_ASSERT(a_token->type == TOKEN_IDENTIFIER, "Wrong key A for type table, expected identifier, got (%d)", a_token->type);
	EE_ASSERT(b_token->type == TOKEN_IDENTIFIER, "Wrong key B for type table, expected identifier, got (%d)", b_token->type);

	if (a_token->scratch.len != b_token->scratch.len)
		return EE_FALSE;

	return memcmp(a_token->scratch.buffer, b_token->scratch.buffer, a_token->scratch.len) == 0;
}

typedef enum Data_Type
{
	DTYPE_U8   = 0,
	DTYPE_U16  = 1,
	DTYPE_U32  = 2,
	DTYPE_U64  = 3,
	DTYPE_I8   = 4,
	DTYPE_I16  = 5,
	DTYPE_I32  = 6,
	DTYPE_I64  = 7,
	DTYPE_F32  = 8,
	DTYPE_F64  = 9,
	DTYPE_VOID = 10,
	DTYPE_BOOL = 11,
	DTYPE_STR  = 12,
	DTYPE_COUNT,
} Data_Type;

typedef enum Type_Flag
{
} Type_Flag;

static const char* _s_dtype_names[DTYPE_COUNT] = {
	"u8", "u16", "u32", "u64",
	"i8", "i16", "i32", "i64",
	"f32", "f64", "void", "bool", "str",
};

static const Usize _s_dtype_lens_names[DTYPE_COUNT] = {
	2, 3, 3, 3,
	2, 3, 3, 3,
	3, 3, 4, 4, 3,
};

static const Usize _s_dtype_sizes[DTYPE_COUNT] = {
	1, 2, 4, 8,
	1, 2, 4, 8,
	4, 8, 
	0, 1, 24
};

static const Usize _s_dtype_aligns[DTYPE_COUNT] = {
	1, 2, 4, 8,
	1, 2, 4, 8,
	4, 8, 
	0, 1, 8
};

static const Token _s_dtype_tokens[DTYPE_COUNT] = {
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "u8",   .len = 2 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "u16",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "u32",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "u64",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "i8",   .len = 2 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "i16",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "i32",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "i64",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "f32",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "f64",  .len = 3 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "void", .len = 4 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "bool", .len = 4 } },
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "str",  .len = 3 } },
};

static const Token _s_anonymous_token = { .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "anonymous", .len = 9 } };

typedef enum Sem_Entry_Type
{
	SEM_ENTRY_FUNC = 0,
	SEM_ENTRY_VAR  = 1,
} Sem_Entry_Type;

typedef struct Sem_Scope Sem_Scope;

typedef struct Sem_Value
{
	Data_Type dtype;

	union
	{
		u8 as_u8; u16 as_u16; u32 as_u32; u64 as_u64;
		i8 as_i8; i16 as_i16; i32 as_i32; i64 as_i64;
		f32 as_f32; f64 as_f64;
	};
} Sem_Value;

typedef struct Sem_Type_Primitive
{
	Data_Type dtype;
} Sem_Type_Primitive;

typedef struct Sem_Struct_Member
{
	const Token* ident;
	struct Sem_Type* type;
	Usize offset;
} Sem_Struct_Member;

typedef struct Sem_Type_Struct
{
	Array members;
} Sem_Type_Struct;

typedef struct Sem_Type_Ptr
{
	struct Sem_Type* to;
} Sem_Type_Ptr;

typedef struct Sem_Type_Func
{
	Array params;
	struct Sem_Type* ret;
} Sem_Type_Func;

typedef struct Sem_Type_Error
{
	const Token* token;
} Sem_Type_Error;

typedef struct Sem_Type
{
	const Token* token;
	
	Ast_Type_Expr_Type type;
	Usize size;
	Usize align;
	u64 flags;

	union
	{
		Sem_Type_Primitive as_primitive;
		Sem_Type_Struct    as_struct;
		Sem_Type_Ptr       as_ptr;
		Sem_Type_Func      as_func;
		Sem_Type_Error     as_error;
	};
} Sem_Type;

typedef struct Sem_Entry
{
	Sem_Entry_Type type;
	const Token* ident;
	Ast_Stmt* decl_stmt;
	Sem_Type* type_info;
	Sem_Value val;
} Sem_Entry;

typedef struct Sem_Scope
{
	Dict types;
	Sem_Scope* parent;
	Array symbols;
	Array children;
	size_t top;
} Sem_Scope;

typedef struct Sem_Analyzer
{
	Ast_Module* mod;
	Sem_Scope* global_scope;
	Allocator allocator;
	Logger log;
	Sem_Type builtin_types[DTYPE_COUNT];
} Sem_Analyzer;

void ee_sem_check_or_panic(Sem_Analyzer* sem, Bool cond, const Token* token, const char* message, ...);

const Token* ee_expr_get_token(const Ast_Expr* expr);

Sem_Entry* ee_alloc_entry(Sem_Analyzer* sem, Sem_Entry_Type type, const Token* ident, Ast_Stmt* decl_stmt, Sem_Type* type_info, Sem_Value val);
Sem_Scope* ee_alloc_scope(Sem_Analyzer* sem, Sem_Scope* parent);
Sem_Entry* ee_scope_lookup_entry(Sem_Scope* scope, const Token* ident);
Sem_Type* ee_scope_lookup_type(Sem_Analyzer* sem, Sem_Scope* scope, const Token* ident);
void ee_scope_set_entry(Sem_Scope* scope, Sem_Entry* entry);
void ee_scope_set_type(Sem_Scope* scope, const Token* ident, Sem_Type* entry);

Sem_Type* ee_sem_create_error_type(Sem_Analyzer* sem, const Token* token);
Sem_Type* ee_sem_get_expr_type(Sem_Analyzer* sem, Ast_Expr* expr, Sem_Scope* scope);
Sem_Type* ee_sem_match_builtin(Sem_Analyzer* sem, const Token* token);
Sem_Type* ee_sem_alloc_type(Sem_Analyzer* sem, Ast_Type_Expr_Type type);
Sem_Type* ee_sem_resolve_type(Sem_Analyzer* sem, Ast_Type* ast_type, Sem_Scope* scope);
void ee_sem_resolve_func_headers(Sem_Analyzer* sem, Ast_Stmt* block, Sem_Scope* scope);
void ee_sem_resolve_scopes(Sem_Analyzer* sem);
void ee_sem_resolve_stmt(Sem_Analyzer* sem, Ast_Stmt* stmt, Sem_Scope* parent, Sem_Type* func_ret_type);
Sem_Analyzer ee_sem_new(Ast_Module* mod, Logger log, const Allocator* allocator);
void ee_sem_debug_print_scope(Sem_Scope* sem, size_t indent);
void ee_sem_debug_print(Sem_Analyzer* sem);

EE_INLINE Bool ee_type_is_int(Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_I8 && type->as_primitive.dtype <= DTYPE_I64;
}

EE_INLINE Bool ee_type_is_uint(const Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_U8 && type->as_primitive.dtype <= DTYPE_U64;
}

EE_INLINE Bool ee_type_is_float(const Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_F32 && type->as_primitive.dtype <= DTYPE_F64;
}

EE_INLINE Bool ee_type_is_bool(const Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_BOOL;
}

EE_INLINE Bool ee_type_is_void(const Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_VOID;
}

EE_INLINE Bool ee_type_is_str(const Sem_Type* type)
{
	return type->type == TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_STR;
}

EE_INLINE Bool ee_types_match(const Sem_Type* a, const Sem_Type* b)
{
	return (ee_type_is_int(a) && ee_type_is_int(b)) || (ee_type_is_uint(a) && ee_type_is_uint(b)) || (ee_type_is_float(a) && ee_type_is_float(b));
}

EE_INLINE Bool ee_sem_type_can_cast(Sem_Type* val, Sem_Type* type)
{
	if (val->type != type->type)
		return EE_FALSE;

	EE_ASSERT(val->type == TYPE_PRIMITIVE && type->type == TYPE_PRIMITIVE, "complex type conversion are not done yet");

	if ((ee_type_is_int(val) || ee_type_is_uint(val) || ee_type_is_bool(val) || ee_type_is_float(val)) &&
		(ee_type_is_int(type) || ee_type_is_uint(type) || ee_type_is_bool(type) || ee_type_is_float(type)))
		return EE_TRUE;

	return EE_FALSE;
}


#endif // EE_SEMANTIC_H