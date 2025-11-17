#include "ee_semantic.h"

static const char* _s_dtype_names[DTYPE_COUNT] = {
	"u8", "u16", "u32", "u64",
	"i8", "i16", "i32", "i64",
	"f32", "f64", "void", "bool", "str",
	"type",
};

static const Usize _s_dtype_sizes[DTYPE_COUNT] = {
	1, 2, 4, 8,
	1, 2, 4, 8,
	4, 8,
	0, 1, 24,
	/* TODO(eesuck): type structure size */ 0,
};

static const Usize _s_dtype_aligns[DTYPE_COUNT] = {
	1, 2, 4, 8,
	1, 2, 4, 8,
	4, 8,
	0, 1, 8,
	/* TODO(eesuck): type structure align */ 0,
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
	{ .type = TOKEN_IDENTIFIER, .scratch = { .buffer = "type", .len = 4 } },
};

static const Token _s_anonymous_token = { .type = TOKEN_IDENTIFIER, .scratch = {.buffer = "anonymous", .len = 9 } };


void ee_sem_resolve_scopes(Sem_Analyzer* sem)
{
	ee_sem_resolve_stmt(sem, sem->mod->root, sem->global_scope, NULL);
}

Sem_Type* ee_sem_alloc_type(Sem_Analyzer* sem, Sem_Type_Kind type)
{
	Sem_Type* type_info = sem->allocator.alloc_fn(&sem->allocator, sizeof(*type_info));
	EE_ASSERT(type_info != NULL, "Unable to allocate memory");

	memset(type_info, 0, sizeof(*type_info));
	type_info->type = type;

	return type_info;
}

Sem_Type* ee_sem_create_error_type(Sem_Analyzer* sem, const Token* token)
{
	Sem_Type* error = ee_sem_alloc_type(sem, SEM_TYPE_ERROR);
	error->as_error.token = token;

	return error;
}

Sem_Type* ee_sem_get_expr_type(Sem_Analyzer* sem, Ast_Expr* expr, Sem_Scope* scope)
{
	switch (expr->type)
	{
	case EXPR_IDENT:
	{
		Sem_Entry* entry = ee_scope_lookup_entry(scope, expr->as_ident.token);

		if (entry != NULL)
		{
			if (entry->type == SEM_ENTRY_VAR || entry->type == SEM_ENTRY_FUNC)
				return entry->type_info;
		}

		ee_log_error_token(&sem->log, expr->as_ident.token, "Use of undeclared variable or value");

		return ee_sem_create_error_type(sem, expr->as_ident.token);
	} break;
	case EXPR_LIT:
	{
		const Token* token = expr->as_lit.token;

		switch (token->type)
		{
		case TOKEN_LIT_INT: return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_I64]);
		case TOKEN_LIT_FLOAT: return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_F64]);
		case TOKEN_LIT_STR: return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_STR]);
		case TOKEN_TRUE: return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_BOOL]);
		case TOKEN_FALSE: return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_BOOL]);
		}

		ee_log_error_token(&sem->log, token, "Unknown literal type");

		return ee_sem_create_error_type(sem, token);
	} break;
	case EXPR_BINOP:
	{
		Sem_Type* left = ee_sem_get_expr_type(sem, expr->as_binop.left, scope);

		if (expr->as_binop.type == BINOP_CAST)
		{
			Ast_Expr* right_expr = expr->as_binop.right;
			Sem_Type* right_type = ee_sem_resolve_type_expr(sem, right_expr, scope);

			if (right_type == NULL)
			{
				const Token* expr_token = ee_expr_get_token(right_expr);
				ee_log_error_token(&sem->log, expr_token, "Unknown type after 'as'");

				return ee_sem_create_error_type(sem, expr_token);
			}

			if (right_type->type == SEM_TYPE_ERROR)
				return right_type;

			if (!ee_sem_type_can_cast(left, right_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Invalid cast");

				return ee_sem_create_error_type(sem, expr_token);
			}

			return right_type;
		}

		Sem_Type* right = ee_sem_get_expr_type(sem, expr->as_binop.right, scope);

		if (left->type == SEM_TYPE_ERROR)
			return left;
		
		if (right->type == SEM_TYPE_ERROR)
			return right;

		switch (expr->as_binop.type)
		{
		case BINOP_PLUS:
		case BINOP_MINUS:
		case BINOP_MUL:
		case BINOP_DIV:
		{
			if (!ee_types_match(left, right))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Invalid operands types for binary operation");

				return ee_sem_create_error_type(sem, expr_token);
			}
			if (left->type != SEM_TYPE_PRIMITIVE)
			{
				ee_log_error_token(&sem->log, ee_expr_get_token(expr), "Binary operations only support primitive types");
				
				return ee_sem_create_error_type(sem, ee_expr_get_token(expr));
			}

			if (left->as_primitive.dtype < right->as_primitive.dtype)
				return right;
			else
				return left;
		} break;
		case BINOP_EQUAL:
		case BINOP_NOT_EQUAL:
		case BINOP_GREATER:
		case BINOP_GREATER_EQUAL:
		case BINOP_LESS:
		case BINOP_LESS_EQUAL:
		{
			if (!ee_types_match(left, right))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Invalid operands types for comparison operation");

				return ee_sem_create_error_type(sem, expr_token);
			}

			return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_BOOL]);
		} break;
		case BINOP_MOD:
		case BINOP_SHIFT_LEFT:
		case BINOP_SHIFT_RIGHT:
		case BINOP_BW_AND:
		case BINOP_BW_OR:
		case BINOP_BW_XOR:
		case BINOP_RANGE:
		{
			if (!ee_types_match(left, right))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Invalid operands types for bitwise/range operation");

				return ee_sem_create_error_type(sem, expr_token);
			}

			if (ee_type_is_float(left) || ee_type_is_float(right))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Bitwise/range operations do not support float operands");

				return ee_sem_create_error_type(sem, expr_token);
			}

			if (left->as_primitive.dtype < right->as_primitive.dtype)
				return right;
			else
				return left;
		} break;
		case BINOP_AND:
		case BINOP_OR:
		{
			if (!ee_type_is_bool(left) || !ee_type_is_bool(right))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Logical operator requires boolean operands");

				return ee_sem_create_error_type(sem, expr_token);
			}
			return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_BOOL]);
		} break;
		default: EE_ASSERT(0, "Unknown operator type (%d)", expr->as_binop.type);
		}
	} break;
	case EXPR_UNOP:
	{
		Sem_Type* op_type = ee_sem_get_expr_type(sem, expr->as_unop.expr, scope);

		if (op_type->type == SEM_TYPE_ERROR)
			return op_type;

		switch (expr->as_unop.type)
		{
		case UNOP_NOT:
		{
			if (!ee_type_is_bool(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Logical NOT '!' operator requires a boolean operand");

				return ee_sem_create_error_type(sem, expr_token);
			}
			return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_BOOL]);
		} break;
		case UNOP_PLUS:
		case UNOP_MINUS:
		{
			if (!ee_type_is_int(op_type) && !ee_type_is_uint(op_type) && !ee_type_is_float(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Unary operator require a numeric operand");

				return ee_sem_create_error_type(sem, expr_token);
			}
			return op_type;
		} break;
		case UNOP_BW_NOT:
		{
			if (!ee_type_is_int(op_type) && !ee_type_is_uint(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Bitwise NOT '~' operator requires an integer operand");

				return ee_sem_create_error_type(sem, expr_token);
			}
			return op_type;
		} break;
		case UNOP_PTR:
		{
			Sem_Type* ptr_type = ee_sem_alloc_type(sem, SEM_TYPE_PTR);

			ptr_type->as_ptr.to = op_type;
			ptr_type->size = 8;
			ptr_type->align = 8;

			return ptr_type;
		} break;
		case UNOP_DEREF:
		{
			if (op_type->type != SEM_TYPE_PTR)
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Dereference '*' operator requires a pointer operand");

				return ee_sem_create_error_type(sem, expr_token);
			}
			return op_type->as_ptr.to;
		} break;
		default: EE_ASSERT(0, "Unknown unary operator type (%d)", expr->as_unop.type);
		}
	} break;
	case EXPR_FUNC_CALL:
	{
		Sem_Type* func = ee_sem_get_expr_type(sem, expr->as_func_call.func, scope);

		if (func->type == SEM_TYPE_ERROR)
			return func;

		if (func->type != SEM_TYPE_FUNC)
		{
			const Token* expr_token = ee_expr_get_token(expr);
			ee_log_error_token(&sem->log, expr_token, "Trying to call non-functional type");

			return ee_sem_create_error_type(sem, expr_token);
		}

		if (ee_array_len(&expr->as_func_call.args) != ee_array_len(&func->as_func.params))
		{
			const Token* expr_token = ee_expr_get_token(expr);
			ee_log_error_token(&sem->log, expr_token, "Expected (%zu) args, but (%zu) were given",
				ee_array_len(&func->as_func.params), ee_array_len(&expr->as_func_call.args));

			return ee_sem_create_error_type(sem, expr_token);
		}

		const Ast_Expr** args = (const Ast_Expr**)expr->as_func_call.args.buffer;
		Sem_Type** params = (Sem_Type**)func->as_func.params.buffer;

		for (size_t i = 0; i < ee_array_len(&func->as_func.params); ++i)
		{
			Sem_Type* arg = ee_sem_get_expr_type(sem, args[i], scope);
			Sem_Type* param = params[i];

			if (arg->type == SEM_TYPE_ERROR)
				return arg;

			if (!ee_types_match(arg, param))
			{
				const Token* expr_token = ee_expr_get_token(expr);
				ee_log_error_token(&sem->log, expr_token, "Type mismatch for argument (%zu)", i);

				return ee_sem_create_error_type(sem, expr_token);
			}
		}

		return func->as_func.ret;
	} break;

	case EXPR_ACCESS:
	{
		Sem_Type* entity_type = ee_sem_get_expr_type(sem, expr->as_access.entity, scope);

		if (entity_type->type == SEM_TYPE_ERROR) 
			return entity_type;

		if (entity_type->type != SEM_TYPE_STRUCT)
		{
			ee_log_error_token(&sem->log, expr->as_access.member, "Access operator '.' can only be used on structs");

			return ee_sem_create_error_type(sem, expr->as_access.member);
		}

		Sem_Struct_Member* members = (Sem_Struct_Member*)entity_type->as_struct.members.buffer;
		for (size_t i = 0; i < ee_array_len(&entity_type->as_struct.members); ++i)
		{
			if (ee_token_scratch_equal(members[i].ident, expr->as_access.member))
			{
				return members[i].type;
			}
		}

		ee_log_error_token(&sem->log, expr->as_access.member, "Struct does not have member with this name");

		return ee_sem_create_error_type(sem, expr->as_access.member);
	} break;

	case EXPR_INDEX:
	{
		Sem_Type* entity_type = ee_sem_get_expr_type(sem, expr->as_index.entity, scope);

		if (entity_type->type == SEM_TYPE_ERROR) 
			return entity_type;

		if (entity_type->type != SEM_TYPE_ARRAY)
		{
			ee_log_error_token(&sem->log, ee_expr_get_token(expr->as_index.index), "Index operator '[]' can only be used on arrays");

			return ee_sem_create_error_type(sem, ee_expr_get_token(expr->as_index.index));
		}

		Sem_Type* index_type = ee_sem_get_expr_type(sem, expr->as_index.index, scope);

		if (index_type->type == SEM_TYPE_ERROR) 
			return index_type;

		if (!ee_type_is_int(index_type) && !ee_type_is_uint(index_type))
		{
			ee_log_error_token(&sem->log, ee_expr_get_token(expr->as_index.index), "Array index must be an integer");

			return ee_sem_create_error_type(sem, ee_expr_get_token(expr->as_index.index));
		}

		return entity_type->as_array.elem_type;
	} break;


	case EXPR_TYPE_STRUCT:
	case EXPR_TYPE_TUPLE:
	case EXPR_TYPE_UNION:
	case EXPR_TYPE_ARRAY:
	{
		return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_TYPE]);
	}

	default: EE_ASSERT(0, "Unknown expression type (%d)", expr->type);
	}

	return ee_sem_create_error_type(sem, ee_expr_get_token(expr));
}

Sem_Type* ee_sem_resolve_type_expr(Sem_Analyzer* sem, Ast_Expr* expr, Sem_Scope* scope)
{
	if (expr == NULL)
	{
		return ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_VOID]);
	}

	switch (expr->type)
	{
	case EXPR_IDENT:
	{
		Sem_Type* out = ee_scope_lookup_type(sem, scope, expr->as_ident.token);
		if (out != NULL)
			return out;

		ee_log_error_token(&sem->log, expr->as_ident.token, "Unknown type name");

		return ee_sem_create_error_type(sem, expr->as_ident.token);
	} break;

	case EXPR_TYPE_STRUCT:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, SEM_TYPE_STRUCT);
		out->as_struct.members = ee_array_new(8, sizeof(Sem_Struct_Member), &sem->allocator);

		const Token** members = (const Token**)expr->as_type_struct.members.buffer;
		Ast_Expr** types = (Ast_Expr**)expr->as_type_struct.types.buffer;
		Usize offset = 0;
		out->align = 1;

		for (size_t i = 0; i < ee_array_len(&expr->as_type_struct.members); ++i)
		{
			Sem_Type* member_type = ee_sem_resolve_type_expr(sem, types[i], scope);
			Sem_Struct_Member struct_member = { 0 };

			if (member_type->type == SEM_TYPE_ERROR)
				continue;

			struct_member.ident = members[i];
			struct_member.type = member_type;

			if (member_type->align > out->align)
				out->align = member_type->align;

			offset = ee_round_up_pow2(offset, member_type->align);
			struct_member.offset = offset;
			offset += member_type->size;

			ee_array_push(&out->as_struct.members, EE_RECAST_U8(struct_member));
		}

		out->token = &_s_anonymous_token;
		out->size = ee_round_up_pow2(offset, out->align);

		return out;
	} break;

	case EXPR_TYPE_TUPLE:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, SEM_TYPE_TUPLE);
		out->as_tuple.types = ee_array_new(8, sizeof(Sem_Type*), &sem->allocator);

		Ast_Expr** types = (Ast_Expr**)expr->as_type_tuple.types.buffer;
		Usize offset = 0;
		out->align = 1;

		for (size_t i = 0; i < ee_array_len(&expr->as_type_tuple.types); ++i)
		{
			Sem_Type* member_type = ee_sem_resolve_type_expr(sem, types[i], scope);
			if (member_type->type == SEM_TYPE_ERROR)
				continue;

			if (member_type->align > out->align)
				out->align = member_type->align;

			offset = ee_round_up_pow2(offset, member_type->align);
			offset += member_type->size;

			ee_array_push(&out->as_tuple.types, EE_RECAST_U8(member_type));
		}

		out->token = &_s_anonymous_token;
		out->size = ee_round_up_pow2(offset, out->align);

		return out;
	} break;

	case EXPR_TYPE_UNION:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, SEM_TYPE_UNION);
		out->as_union.types = ee_array_new(8, sizeof(Sem_Type*), &sem->allocator);
		Ast_Expr** types = (Ast_Expr**)expr->as_type_union.types.buffer;
		out->align = 1;
		out->size = 0;

		for (size_t i = 0; i < ee_array_len(&expr->as_type_union.types); ++i)
		{
			Sem_Type* member_type = ee_sem_resolve_type_expr(sem, types[i], scope);
			if (member_type->type == SEM_TYPE_ERROR)
				continue;

			if (member_type->align > out->align)
				out->align = member_type->align;

			if (member_type->size > out->size)
				out->size = member_type->size;

			ee_array_push(&out->as_union.types, EE_RECAST_U8(member_type));
		}

		out->token = &_s_anonymous_token;
		out->size = ee_round_up_pow2(out->size, out->align);

		return out;
	} break;

	case EXPR_TYPE_ARRAY:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, SEM_TYPE_ARRAY);
		Sem_Type* size_type = ee_sem_get_expr_type(sem, expr->as_type_array.size, scope);

		if (size_type->type == SEM_TYPE_ERROR) 
			return size_type;

		if (!ee_type_is_int(size_type) && !ee_type_is_uint(size_type))
		{
			ee_log_error_token(&sem->log, ee_expr_get_token(expr->as_type_array.size), "Array size must be an integer");

			return ee_sem_create_error_type(sem, ee_expr_get_token(expr->as_type_array.size));
		}

		out->as_array.elem_type = ee_sem_resolve_type_expr(sem, expr->as_type_array.type_expr, scope);

		if (out->as_array.elem_type->type == SEM_TYPE_ERROR)
			return out->as_array.elem_type;

		out->align = out->as_array.elem_type->align;

		return out;
	} break;

	case EXPR_UNOP:
	{
		if (expr->as_unop.type == UNOP_PTR)
		{
			Sem_Type* out = ee_sem_alloc_type(sem, SEM_TYPE_PTR);

			out->token = &_s_anonymous_token;
			out->size = 8;
			out->align = 8;
			out->as_ptr.to = ee_sem_resolve_type_expr(sem, expr->as_unop.expr, scope);

			return out;
		}
	}
	default:
		ee_log_error_token(&sem->log, ee_expr_get_token(expr), "Type expression expected, but got a value expression");

		return ee_sem_create_error_type(sem, ee_expr_get_token(expr));
	}
}

void ee_sem_resolve_func_headers(Sem_Analyzer* sem, Ast_Stmt* block, Sem_Scope* scope)
{
	Ast_Stmt** stmts = (Ast_Stmt**)block->as_block.stmts.buffer;

	for (size_t i = 0; i < ee_array_len(&block->as_block.stmts); ++i)
	{
		Ast_Stmt* stmt = stmts[i];

		if (stmt->type != STMT_FN)
		{
			continue;
		}

		Sem_Type* func_type = ee_sem_alloc_type(sem, SEM_TYPE_FUNC);
		func_type->as_func.params = ee_array_new(8, sizeof(Sem_Type*), &sem->allocator);

		Ast_Expr** param_type_exprs = (Ast_Expr**)stmt->as_func_decl.param_types.buffer;

		for (size_t j = 0; j < ee_array_len(&stmt->as_func_decl.param_types); ++j)
		{
			Sem_Type* param_type = ee_sem_resolve_type_expr(sem, param_type_exprs[j], scope);
			ee_array_push(&func_type->as_func.params, EE_RECAST_U8(param_type));
		}

		if (stmt->as_func_decl.ret_type != NULL)
		{
			func_type->as_func.ret = ee_sem_resolve_type_expr(sem, stmt->as_func_decl.ret_type, scope);
		}
		else
		{
			func_type->as_func.ret = ee_scope_lookup_type(sem, scope, &_s_dtype_tokens[DTYPE_VOID]);
		}

		func_type->token = stmt->as_func_decl.ident;
		func_type->size = 8;
		func_type->align = 8;

		Sem_Entry* func_entry = ee_alloc_entry(sem, SEM_ENTRY_FUNC, stmt->as_func_decl.ident, stmt, func_type, (Sem_Value) { 0 });

		ee_sem_check_or_panic(sem, ee_scope_lookup_entry(scope, stmt->as_func_decl.ident) == NULL, stmt->as_func_decl.ident,
			"Trying to redefine an existing function");

		ee_scope_set_entry(scope, func_entry);
	}
}

void ee_sem_resolve_stmt(Sem_Analyzer* sem, Ast_Stmt* stmt, Sem_Scope* parent, Sem_Type* func_ret_type)
{
	switch (stmt->type)
	{
	case STMT_VAR_DECL:
	{
		Sem_Type* declared_type = NULL;
		Sem_Type* value_type = NULL;
		Sem_Type* final_type = NULL;

		if (stmt->as_var_decl.val != NULL)
		{
			value_type = ee_sem_get_expr_type(sem, stmt->as_var_decl.val, parent);

			if (value_type->type == SEM_TYPE_ERROR)
				return;
		}

		if (stmt->as_var_decl.type_expr != NULL)
		{
			declared_type = ee_sem_resolve_type_expr(sem, stmt->as_var_decl.type_expr, parent);

			if (declared_type->type == SEM_TYPE_ERROR)
				return;
		}

		if (declared_type != NULL && value_type != NULL)
		{
			if (!ee_sem_type_can_cast(declared_type, value_type))
			{
				const Token* expr_token = ee_expr_get_token(stmt->as_var_decl.val);
				ee_log_error_token(&sem->log, expr_token, "Type mismatch in variable declaration");
				final_type = ee_sem_create_error_type(sem, expr_token);
			}
			else
			{
				final_type = declared_type;
			}
		}
		else if (value_type != NULL)
		{
			final_type = value_type;
		}
		else if (declared_type != NULL)
		{
			final_type = declared_type;
		}
		else
		{
			ee_log_error_token(&sem->log, stmt->as_var_decl.ident, "Cannot infer type for variable");
			final_type = ee_sem_create_error_type(sem, stmt->as_var_decl.ident);
		}

		Sem_Entry* var_entry = ee_alloc_entry(sem, SEM_ENTRY_VAR, stmt->as_var_decl.ident, stmt, final_type, (Sem_Value) { 0 });

		ee_sem_check_or_panic(sem, ee_scope_lookup_entry(parent, stmt->as_var_decl.ident) == NULL, stmt->as_var_decl.ident,
			"Trying to redefine an existing variable");

		ee_scope_set_entry(parent, var_entry);
	} break;
	case STMT_FN:
	{
		Sem_Entry* func_entry = ee_scope_lookup_entry(parent, stmt->as_func_decl.ident);
		ee_sem_check_or_panic(sem, func_entry != NULL, stmt->as_func_decl.ident, "Unresolved function entry. Did you forget to run resolve_func_headers?");

		Sem_Type* func_type = func_entry->type_info;
		EE_ASSERT(func_type->type == SEM_TYPE_FUNC, "Function entry is not a function type");

		Sem_Scope* func_scope = ee_alloc_scope(sem, parent);

		const Token** params = (const Token**)stmt->as_func_decl.param_idents.buffer;
		Sem_Type** param_types = (Sem_Type**)func_type->as_func.params.buffer;

		for (size_t i = 0; i < ee_array_len(&stmt->as_func_decl.param_idents); ++i)
		{
			Sem_Type* sem_type = param_types[i];
			Sem_Entry* param_entry = ee_alloc_entry(sem, SEM_ENTRY_VAR, params[i], stmt, sem_type, (Sem_Value) { 0 });

			ee_scope_set_entry(func_scope, param_entry);
		}

		ee_sem_resolve_stmt(sem, stmt->as_func_decl.body, func_scope, func_type->as_func.ret);
	} break;
	case STMT_BLOCK:
	{
		ee_sem_resolve_func_headers(sem, stmt, parent);

		Sem_Scope* block_scope = ee_alloc_scope(sem, parent);
		Ast_Stmt** block_stmts = (Ast_Stmt**)stmt->as_block.stmts.buffer;

		for (size_t i = 0; i < ee_array_len(&stmt->as_block.stmts); ++i)
		{
			Ast_Stmt* block_stmt = block_stmts[i];
			ee_sem_resolve_stmt(sem, block_stmt, block_scope, func_ret_type);
		}
	} break;
	case STMT_IF:
	{
		Sem_Type* cond_type = ee_sem_get_expr_type(sem, stmt->as_if.cond, parent);

		if (cond_type->type != SEM_TYPE_ERROR && !ee_type_is_bool(cond_type))
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_if.cond);
			ee_log_error_token(&sem->log, expr_token, "Invalid condition expression type, expected bool");
		}

		ee_sem_resolve_stmt(sem, stmt->as_if.if_block, parent, func_ret_type);

		if (stmt->as_if.else_block != NULL)
		{
			ee_sem_resolve_stmt(sem, stmt->as_if.else_block, parent, func_ret_type);
		}
	} break;
	case STMT_FOR:
	{
		Sem_Type* range_type = ee_sem_get_expr_type(sem, stmt->as_for.range, parent);
		Sem_Type* it_type = NULL;

		if (range_type->type == SEM_TYPE_ERROR)
		{
			it_type = range_type;
		}
		else if (ee_type_is_int(range_type) || ee_type_is_uint(range_type))
		{
			it_type = range_type;
		}
		else
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_for.range);
			ee_log_error_token(&sem->log, expr_token, "For loop range must be an integer type");

			it_type = ee_sem_create_error_type(sem, expr_token);
		}

		Sem_Scope* for_scope = ee_alloc_scope(sem, parent);
		Sem_Entry* it_entry = ee_alloc_entry(sem, SEM_ENTRY_VAR, stmt->as_for.it, stmt, it_type, (Sem_Value) { 0 });

		ee_scope_set_entry(for_scope, it_entry);
		ee_sem_resolve_stmt(sem, stmt->as_for.body, for_scope, func_ret_type);
	} break;
	case STMT_WHILE:
	{
		Sem_Type* cond_type = ee_sem_get_expr_type(sem, stmt->as_while.cond, parent);

		if (cond_type->type != SEM_TYPE_ERROR && !ee_type_is_bool(cond_type))
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_while.cond);
			ee_log_error_token(&sem->log, expr_token, "Invalid condition expression type, expected bool");
		}

		ee_sem_resolve_stmt(sem, stmt->as_while.body, parent, func_ret_type);
	} break;
	case STMT_EXPR:
	{
		ee_sem_get_expr_type(sem, stmt->as_expr.expr, parent);
	} break;
	case STMT_RETURN:
	{
		if (func_ret_type == NULL)
		{
			if (stmt->as_ret.val != NULL)
			{
				const Token* expr_token = ee_expr_get_token(stmt->as_ret.val);
				ee_sem_check_or_panic(sem, 0, expr_token, "Trying to return from outside a function");
			}
			else
			{
				ee_sem_check_or_panic(sem, 0, NULL, "Trying to return from outside a function");
			}
		}

		Sem_Type* ret_type = ee_scope_lookup_type(sem, parent, &_s_dtype_tokens[DTYPE_VOID]);

		if (stmt->as_ret.val != NULL)
			ret_type = ee_sem_get_expr_type(sem, stmt->as_ret.val, parent);

		if (ret_type->type != SEM_TYPE_ERROR && !ee_types_match(ret_type, func_ret_type))
		{
			if (stmt->as_ret.val != NULL)
			{
				const Token* expr_token = ee_expr_get_token(stmt->as_ret.val);
				ee_sem_check_or_panic(sem, 0, expr_token, "Invalid return value type");
			}
			else
			{
				ee_sem_check_or_panic(sem, 0, NULL, "Invalid return value type");
			}
		}
	} break;
	case STMT_ASSIGN:
	{
		Ast_Expr* lvalue = stmt->as_assign.ident;
		Ast_Expr* rvalue = stmt->as_assign.val;
		Bool is_assignable = EE_FALSE;

		switch (lvalue->type)
		{
		case EXPR_IDENT:
		{
			Sem_Entry* entry = ee_scope_lookup_entry(parent, lvalue->as_ident.token);
			if (entry == NULL)
			{
				ee_log_error_token(&sem->log, lvalue->as_ident.token, "Assignment to undeclared variable");
				break;
			}

			if (entry->decl_stmt->type == STMT_VAR_DECL &&
				ee_var_decl_has_flag(entry->decl_stmt, AST_CONST))
			{
				ee_log_error_token(&sem->log, lvalue->as_ident.token, "Cannot assign to a constant variable");
				break;
			}

			is_assignable = EE_TRUE;
			break;
		}
		case EXPR_ACCESS:
		case EXPR_INDEX:
		{
			is_assignable = EE_TRUE;
			break;
		}
		case EXPR_UNOP:
		{
			if (lvalue->as_unop.type == UNOP_DEREF)
			{
				is_assignable = EE_TRUE;
			}
			else
			{
				ee_log_error_token(&sem->log, ee_expr_get_token(lvalue),
					"Invalid left-hand side in assignment (expression is not an l-value)");
			}
			break;
		}
		default:
		{
			ee_log_error_token(&sem->log, ee_expr_get_token(lvalue),
				"Invalid left-hand side in assignment (not an l-value)");
		} break;
		}

		if (!is_assignable)
		{
			break;
		}

		Sem_Type* lhs_type = ee_sem_get_expr_type(sem, lvalue, parent);
		Sem_Type* rhs_type = ee_sem_get_expr_type(sem, rvalue, parent);

		if (lhs_type->type == SEM_TYPE_ERROR || rhs_type->type == SEM_TYPE_ERROR)
		{
			break;
		}

		if (!ee_types_match(lhs_type, rhs_type))
		{
			const Token* expr_token = ee_expr_get_token(rvalue);
			ee_log_error_token(&sem->log, expr_token, "Type mismatch: cannot assign value");
		}
	} break;
	}
}

void ee_sem_check_or_panic(Sem_Analyzer* sem, Bool cond, const Token* token, const char* message, ...)
{
	va_list args;
	va_start(args, message);

	if (!cond)
	{
		ee_log_error_token_va(&sem->log, token, message, args);
	}

	va_end(args);
}

const Token* ee_expr_get_token(const Ast_Expr* expr)
{
	if (expr == NULL)
		return NULL;

	switch (expr->type)
	{
	case EXPR_LIT: return expr->as_lit.token;
	case EXPR_IDENT: return expr->as_ident.token;
	case EXPR_BINOP: return ee_expr_get_token(expr->as_binop.left);
	case EXPR_UNOP: return ee_expr_get_token(expr->as_unop.expr);
	case EXPR_FUNC_CALL: return ee_expr_get_token(expr->as_func_call.func);
	case EXPR_ACCESS: return ee_expr_get_token(expr->as_access.entity);
	case EXPR_INDEX: return ee_expr_get_token(expr->as_index.entity);

	case EXPR_TYPE_STRUCT:
	{
		if (ee_array_len(&expr->as_type_struct.members) > 0)
		{
			Token** members = (Token**)expr->as_type_struct.members.buffer;
			return members[0];
		}
		return NULL;
	}
	case EXPR_TYPE_TUPLE:
	{
		if (ee_array_len(&expr->as_type_tuple.types) > 0)
		{
			Ast_Expr** types = (Ast_Expr**)expr->as_type_tuple.types.buffer;
			return ee_expr_get_token(types[0]);
		}
		return NULL;
	}
	case EXPR_TYPE_UNION:
	{
		if (ee_array_len(&expr->as_type_union.types) > 0)
		{
			Ast_Expr** types = (Ast_Expr**)expr->as_type_union.types.buffer;
			return ee_expr_get_token(types[0]);
		}
		return NULL;
	}
	case EXPR_TYPE_ARRAY: return ee_expr_get_token(expr->as_type_array.size);
	}

	return NULL;
}

Sem_Entry* ee_alloc_entry(Sem_Analyzer* sem, Sem_Entry_Type type, const Token* ident, Ast_Stmt* decl_stmt, Sem_Type* type_info, Sem_Value val)
{
	Sem_Entry* entry = sem->allocator.alloc_fn(&sem->allocator, sizeof(*entry));
	EE_ASSERT(entry != NULL, "Unable to allocate memory");

	entry->type = type;
	entry->ident = ident;
	entry->type_info = type_info;
	entry->decl_stmt = decl_stmt;
	entry->val = val;

	return entry;
}

Sem_Scope* ee_alloc_scope(Sem_Analyzer* sem, Sem_Scope* parent)
{
	Sem_Scope* out = sem->allocator.alloc_fn(&sem->allocator, sizeof(*out));
	EE_ASSERT(out != NULL, "Unable to allocate memory");

	DictConfig config = ee_dict_config_new(&sem->allocator, ee_token_ptr_hash, ee_token_ptr_eq, NULL, NULL);

	out->parent = parent;
	out->children = ee_array_new(32, sizeof(Sem_Scope*), &sem->allocator);
	out->symbols = ee_array_new(32, sizeof(Sem_Entry), &sem->allocator);
	out->types = ee_dict_new(32, sizeof(const Token*), sizeof(Sem_Type*), config);
	out->top = 0;

	if (parent != NULL)
		ee_array_push(&parent->children, EE_RECAST_U8(out));

	return out;
}

Sem_Entry* ee_scope_lookup_entry(Sem_Scope* scope, const Token* ident)
{
	if (ident == NULL || ident->type == TOKEN_INVALID)
		return NULL;

	while (scope)
	{
		Sem_Entry* entries = (Sem_Entry*)scope->symbols.buffer;

		for (size_t i = 0; i < ee_array_len(&scope->symbols); ++i)
		{
			if (entries[i].ident->scratch.len == ident->scratch.len &&
				memcmp(entries[i].ident->scratch.buffer, ident->scratch.buffer, ident->scratch.len) == 0)
			{
				return &entries[i];
			}
		}

		scope = scope->parent;
	}

	return NULL;
}

Sem_Type* ee_scope_lookup_type(Sem_Analyzer* sem, Sem_Scope* scope, const Token* ident)
{
	if (ident == NULL || ident->type == TOKEN_INVALID)
		return NULL;

	while (scope)
	{
		if (ee_dict_contains(&scope->types, EE_RECAST_U8(ident)))
			return *(Sem_Type**)ee_dict_at(&scope->types, EE_RECAST_U8(ident));

		scope = scope->parent;
	}

	return NULL;
}

void ee_scope_set_entry(Sem_Scope* scope, Sem_Entry* entry)
{
	ee_array_push(&scope->symbols, (const u8*)entry);
}

void ee_scope_set_type(Sem_Scope* scope, const Token* ident, Sem_Type* entry)
{
	EE_ASSERT(ident != NULL, "Trying to set NULL identifier");
	ee_dict_set(&scope->types, EE_RECAST_U8(ident), EE_RECAST_U8(entry));
}

Sem_Analyzer ee_sem_new(Ast_Module* mod, Logger log, const Allocator* allocator)
{
	Sem_Analyzer out = { 0 };

	if (allocator == NULL)
	{
		out.allocator.alloc_fn = ee_default_alloc;
		out.allocator.realloc_fn = ee_default_realloc;
		out.allocator.free_fn = ee_default_free;
		out.allocator.context = NULL;
	}
	else
	{
		memcpy(&out.allocator, allocator, sizeof(Allocator));
	}

	EE_ASSERT(out.allocator.alloc_fn != NULL, "Trying to set NULL alloc callback");
	EE_ASSERT(out.allocator.realloc_fn != NULL, "Trying to set NULL realloc callback");
	EE_ASSERT(out.allocator.free_fn != NULL, "Trying to set NULL free callback");

	out.global_scope = ee_alloc_scope(&out, NULL);
	out.mod = mod;
	out.log = log;

	for (Data_Type dtype = 0; dtype < DTYPE_COUNT; ++dtype)
	{
		Sem_Type* ptype = ee_sem_alloc_type(&out, SEM_TYPE_PRIMITIVE);

		ptype->size = _s_dtype_sizes[dtype];
		ptype->align = _s_dtype_aligns[dtype];
		ptype->token = &_s_dtype_tokens[dtype];
		ptype->as_primitive.dtype = dtype;

		ee_scope_set_type(out.global_scope, ptype->token, ptype);
	}

	return out;
}

void ee_sem_debug_print_scope(Sem_Scope* sem, size_t indent)
{
	ee_println_with_indent(indent, "SCOPE: ");

	if (ee_dict_count(&sem->symbols) > 0)
		ee_println_with_indent(indent, "SYMBOLS: ");

	Sem_Entry* entries = (Sem_Entry*)sem->symbols.buffer;

	for (size_t i = 0; i < ee_array_len(&sem->symbols); ++i)
	{
		const Token* symbol = entries[i].ident;

		if (entries[i].type_info->type == SEM_TYPE_PRIMITIVE)
			ee_print_with_indent(indent + 1, "TYPE (%s) ", _s_dtype_names[entries[i].type_info->as_primitive.dtype]);
		else
			ee_print_with_indent(indent + 1, "COMPLEX_TYPE ");

		EE_PRINTLN("ENTRY '%.*s'", (u32)symbol->scratch.len, symbol->scratch.buffer);
	}

	ee_println_with_indent(indent, "SUB_SCOPES: ");
	Sem_Scope** scopes = (Sem_Scope**)sem->children.buffer;

	for (size_t i = 0; i < ee_array_len(&sem->children); ++i)
	{
		ee_sem_debug_print_scope(scopes[i], indent + 1);
	}
}

void ee_sem_debug_print(Sem_Analyzer* sem)
{
	ee_sem_debug_print_scope(sem->global_scope, 0);
}