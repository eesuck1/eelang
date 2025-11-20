#ifndef EE_SEMANTIC_H
#define EE_SEMANTIC_H

#include "ee_parser.h"
#include "ee_dict.h"

typedef enum Data_Type
{
	DTYPE_U8 = 0,
	DTYPE_U16 = 1,
	DTYPE_U32 = 2,
	DTYPE_U64 = 3,
	DTYPE_I8 = 4,
	DTYPE_I16 = 5,
	DTYPE_I32 = 6,
	DTYPE_I64 = 7,
	DTYPE_F32 = 8,
	DTYPE_F64 = 9,
	DTYPE_VOID = 10,
	DTYPE_BOOL = 11,
	DTYPE_STR = 12,
	DTYPE_TYPE = 13,
	DTYPE_COUNT,
} Data_Type;

typedef enum Type_Flag
{
} Type_Flag;

typedef enum Sem_Entry_Type
{
	SEM_ENTRY_FUNC = 0,
	SEM_ENTRY_VAR = 1,
} Sem_Entry_Type;

typedef struct Sem_Scope Sem_Scope;
typedef struct Sem_Type Sem_Type;

typedef struct Sem_Value
{
	Data_Type dtype;

	union
	{
		u8 as_u8; u16 as_u16; u32 as_u32; u64 as_u64;
		i8 as_i8; i16 as_i16; i32 as_i32; i64 as_i64;
		f32 as_f32; f64 as_f64;
		Sem_Type* as_type;
	};
} Sem_Value;

typedef enum Sem_Type_Kind
{
	SEM_TYPE_ERROR,
	SEM_TYPE_PRIMITIVE,
	SEM_TYPE_STRUCT,
	SEM_TYPE_TUPLE,
	SEM_TYPE_UNION,
	SEM_TYPE_ARRAY,
	SEM_TYPE_PTR,
	SEM_TYPE_FUNC,
} Sem_Type_Kind;

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

typedef struct Sem_Type_Tuple
{
	Array types;
} Sem_Type_Tuple;

typedef struct Sem_Type_Union
{
	Array types;
} Sem_Type_Union;

typedef struct Sem_Type_Array
{
	struct Sem_Type* elem_type;
	u64 count;
} Sem_Type_Array;

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

struct Sem_Type
{
	const Token* token;

	Sem_Type_Kind type;
	Usize size;
	Usize align;
	u64 flags;

	union
	{
		Sem_Type_Primitive as_primitive;
		Sem_Type_Struct    as_struct;
		Sem_Type_Tuple     as_tuple;
		Sem_Type_Union     as_union;
		Sem_Type_Array     as_array;
		Sem_Type_Ptr       as_ptr;
		Sem_Type_Func      as_func;
		Sem_Type_Error     as_error;
	};
};

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
} Sem_Analyzer;

void ee_sem_check_or_panic(Sem_Analyzer* sem, Bool cond, const Token* token, const char* message, ...);
const Token* ee_expr_get_token(const Ast_Expr* expr);

Sem_Entry* ee_alloc_entry(Sem_Analyzer* sem, Sem_Entry_Type type, const Token* ident, Ast_Stmt* decl_stmt, Sem_Type* type_info, Sem_Value val);
Sem_Scope* ee_alloc_scope(Sem_Analyzer* sem, Sem_Scope* parent);
Sem_Entry* ee_scope_lookup_entry(Sem_Scope* scope, const Token* ident);
void ee_scope_set_entry(Sem_Scope* scope, Sem_Entry* entry);

Sem_Type* ee_sem_create_error_type(Sem_Analyzer* sem, const Token* token);
Sem_Type* ee_sem_get_expr_type(Sem_Analyzer* sem, Ast_Expr* expr, Sem_Scope* scope);
Sem_Type* ee_sem_alloc_type(Sem_Analyzer* sem, Sem_Type_Kind type);
Sem_Type* ee_sem_resolve_type_expr(Sem_Analyzer* sem, Ast_Expr* expr, Sem_Scope* scope);

void ee_sem_resolve_func_headers(Sem_Analyzer* sem, Ast_Stmt* block, Sem_Scope* scope);
void ee_sem_resolve_scopes(Sem_Analyzer* sem);
void ee_sem_resolve_stmt(Sem_Analyzer* sem, Ast_Stmt* stmt, Sem_Scope* parent, Sem_Type* func_ret_type);

Sem_Analyzer ee_sem_new(Ast_Module* mod, Logger log, const Allocator* allocator);
void ee_sem_debug_print_scope(Sem_Scope* sem, size_t indent);
void ee_sem_debug_print(Sem_Analyzer* sem);

EE_INLINE Bool ee_type_is_int(Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_I8 && type->as_primitive.dtype <= DTYPE_I64;
}

EE_INLINE Bool ee_type_is_uint(const Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_U8 && type->as_primitive.dtype <= DTYPE_U64;
}

EE_INLINE Bool ee_type_is_float(const Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype >= DTYPE_F32 && type->as_primitive.dtype <= DTYPE_F64;
}

EE_INLINE Bool ee_type_is_bool(const Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_BOOL;
}

EE_INLINE Bool ee_type_is_void(const Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_VOID;
}

EE_INLINE Bool ee_type_is_str(const Sem_Type* type)
{
	return type->type == SEM_TYPE_PRIMITIVE && type->as_primitive.dtype == DTYPE_STR;
}

EE_INLINE Bool ee_types_match(const Sem_Type* a, const Sem_Type* b)
{
	if (a == b)
		return EE_TRUE;

	if (a == NULL || b == NULL)
		return EE_FALSE;

	if (a->type != b->type)
		return EE_FALSE;

	switch (a->type)
	{
	case SEM_TYPE_PRIMITIVE:
	{
		return a->as_primitive.dtype == b->as_primitive.dtype;
	}

	case SEM_TYPE_PTR:
	{
		return ee_types_match(a->as_ptr.to, b->as_ptr.to);
	}

	case SEM_TYPE_ARRAY:
	{
		if (a->as_array.count != b->as_array.count)
			return EE_FALSE;
		return ee_types_match(a->as_array.elem_type, b->as_array.elem_type);
	}

	case SEM_TYPE_TUPLE:
	{
		if (ee_array_len(&a->as_tuple.types) != ee_array_len(&b->as_tuple.types))
			return EE_FALSE;

		Sem_Type** a_types = (Sem_Type**)a->as_tuple.types.buffer;
		Sem_Type** b_types = (Sem_Type**)b->as_tuple.types.buffer;

		for (size_t i = 0; i < ee_array_len(&a->as_tuple.types); ++i)
		{
			if (!ee_types_match(a_types[i], b_types[i]))
				return EE_FALSE;
		}
		return EE_TRUE;
	}

	case SEM_TYPE_STRUCT:
	{
		if (ee_array_len(&a->as_struct.members) != ee_array_len(&b->as_struct.members))
			return EE_FALSE;

		Sem_Struct_Member* a_members = (Sem_Struct_Member*)a->as_struct.members.buffer;
		Sem_Struct_Member* b_members = (Sem_Struct_Member*)b->as_struct.members.buffer;

		for (size_t i = 0; i < ee_array_len(&a->as_struct.members); ++i)
		{
			if (!ee_token_scratch_equal(a_members[i].ident, b_members[i].ident))
				return EE_FALSE;

			if (!ee_types_match(a_members[i].type, b_members[i].type))
				return EE_FALSE;
		}
		return EE_TRUE;
	}

	case SEM_TYPE_UNION:
	{
		if (ee_array_len(&a->as_union.types) != ee_array_len(&b->as_union.types))
			return EE_FALSE;

		Sem_Type** a_types = (Sem_Type**)a->as_union.types.buffer;
		Sem_Type** b_types = (Sem_Type**)b->as_union.types.buffer;

		for (size_t i = 0; i < ee_array_len(&a->as_union.types); ++i)
		{
			if (!ee_types_match(a_types[i], b_types[i]))
				return EE_FALSE;
		}
		return EE_TRUE;
	}

	case SEM_TYPE_FUNC:
	{
		if (!ee_types_match(a->as_func.ret, b->as_func.ret))
			return EE_FALSE;

		if (ee_array_len(&a->as_func.params) != ee_array_len(&b->as_func.params))
			return EE_FALSE;

		Sem_Type** a_params = (Sem_Type**)a->as_func.params.buffer;
		Sem_Type** b_params = (Sem_Type**)b->as_func.params.buffer;

		for (size_t i = 0; i < ee_array_len(&a->as_func.params); ++i)
		{
			if (!ee_types_match(a_params[i], b_params[i]))
				return EE_FALSE;
		}
		return EE_TRUE;
	}

	case SEM_TYPE_ERROR:
		return EE_FALSE;
	}

	return EE_FALSE;
}

EE_INLINE Bool ee_sem_is_implicitly_convertible_to(Sem_Type* from, Sem_Type* to)
{
	if (ee_types_match(from, to))
		return EE_TRUE;

	if (from->type != SEM_TYPE_PRIMITIVE || to->type != SEM_TYPE_PRIMITIVE)
		return EE_FALSE;

	Data_Type from_dt = from->as_primitive.dtype;
	Data_Type to_dt = to->as_primitive.dtype;

	if (ee_type_is_int(from) && ee_type_is_int(to))
	{
		return from_dt <= to_dt;
	}

	if (ee_type_is_uint(from) && ee_type_is_uint(to))
	{
		return from_dt <= to_dt;
	}

	if (ee_type_is_float(from) && ee_type_is_float(to))
	{
		return from_dt <= to_dt;
	}

	if ((ee_type_is_int(from) || ee_type_is_uint(from)) && ee_type_is_float(to))
	{
		return EE_TRUE;
	}

	return EE_FALSE;
}

EE_INLINE Bool ee_sem_type_can_cast(Sem_Type* from, Sem_Type* to)
{
	if (from->type == SEM_TYPE_PRIMITIVE && to->type == SEM_TYPE_PRIMITIVE)
	{
		return (ee_type_is_int(from) || ee_type_is_uint(from) || ee_type_is_bool(from) || ee_type_is_float(from)) &&
			(ee_type_is_int(to) || ee_type_is_uint(to) || ee_type_is_bool(to) || ee_type_is_float(to));
	}

	if (from->type == SEM_TYPE_PTR && to->type == SEM_TYPE_PTR)
	{
		return EE_TRUE;
	}

	if (from->type == SEM_TYPE_PTR && (ee_type_is_int(to) || ee_type_is_uint(to)))
	{
		return EE_TRUE;
	}

	if ((ee_type_is_int(from) || ee_type_is_uint(from)) && to->type == SEM_TYPE_PTR)
	{
		return EE_TRUE;
	}

	return EE_FALSE;
}

#endif