#include "ee_semantic.h"

void ee_sem_resolve_scopes(Sem_Analyzer* sem)
{
	ee_sem_resolve_stmt(sem, sem->mod->root, sem->global_scope, NULL);
}

Sem_Type* ee_sem_match_builtin(Sem_Analyzer* sem, const Token* token)
{
	if (token->type == TOKEN_INVALID)
		return NULL;

	for (Data_Type dtype = 0; dtype < DTYPE_COUNT; ++dtype)
	{
		if (ee_token_scratch_equal(token, sem->builtin_types[dtype].token))
			return &sem->builtin_types[dtype];
	}

	return NULL;
}

Sem_Type* ee_sem_alloc_type(Sem_Analyzer* sem, Ast_Type_Expr_Type type)
{
	Sem_Type* type_info = sem->allocator.alloc_fn(&sem->allocator, sizeof(*type_info));
	EE_ASSERT(type_info != NULL, "Unable to allocate memory");

	memset(type_info, 0, sizeof(*type_info));
	type_info->type = type;

	return type_info;
}

Sem_Type* ee_sem_create_error_type(Sem_Analyzer* sem, const Token* token)
{
	Sem_Type* error = ee_sem_alloc_type(sem, TYPE_ERROR);
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
			return entry->type_info;

		ee_log_error_token(&sem->log, expr->as_ident.token, "Use of undeclared variable");
		return ee_sem_create_error_type(sem, expr->as_ident.token);
	} break;
	case EXPR_LIT:
	{
		const Token* token = expr->as_lit.token;

		switch (token->type)
		{
		case TOKEN_LIT_INT: return &sem->builtin_types[DTYPE_I64];
		case TOKEN_LIT_FLOAT: return &sem->builtin_types[DTYPE_F64];
		case TOKEN_LIT_STR: return &sem->builtin_types[DTYPE_STR];
		case TOKEN_TRUE: return &sem->builtin_types[DTYPE_BOOL];
		case TOKEN_FALSE: return &sem->builtin_types[DTYPE_BOOL];
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
			Sem_Type* right_type = NULL;

			if (right_expr->type != EXPR_IDENT)
			{
				const Token* expr_token = ee_expr_get_token(right_expr);

				ee_log_error_token(&sem->log, expr_token, "Type name expected after 'as'");
				return ee_sem_create_error_type(sem, expr_token);
			}

			right_type = ee_scope_lookup_type(sem, scope, right_expr->as_ident.token);

			if (right_type == NULL)
			{
				const Token* expr_token = ee_expr_get_token(right_expr);

				ee_log_error_token(&sem->log, expr_token, "Unknown type after 'as'");
				return ee_sem_create_error_type(sem, expr_token);
			}

			if (!ee_sem_type_can_cast(left, right_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Invalid cast from type (%s) to (%s)",
					_s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right_type->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return right_type;
		}

		Sem_Type* right = ee_sem_get_expr_type(sem, expr->as_binop.right, scope);

		if (left->type == TYPE_ERROR)
			return left;
		else if (right->type == TYPE_ERROR)
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

				ee_log_error_token(&sem->log, expr_token,
					"Invalid operands types for operation (%s), type of the left (%s) does not match with type of the right (%s)",
					_s_op_binop_name_table[expr->as_binop.type], _s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
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
				
				ee_log_error_token(&sem->log, expr_token,
					"Invalid operands types for operation (%s), type of the left (%s) does not match with type of the right (%s)",
					_s_op_binop_name_table[expr->as_binop.type], _s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return &sem->builtin_types[DTYPE_BOOL];
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

				ee_log_error_token(&sem->log, expr_token,
					"Invalid operands types for operation (%s), type of the left (%s) does not match with type of the right (%s)", 
					_s_op_binop_name_table[expr->as_binop.type], _s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			if (ee_type_is_float(left))
			{
				const Token* expr_token = ee_expr_get_token(expr->as_binop.left);

				ee_log_error_token(&sem->log, expr_token,
					"Invalid operand type (%s) for operation (%s)",
					_s_dtype_names[left->as_primitive.dtype], _s_op_binop_name_table[expr->as_binop.type]);

				return ee_sem_create_error_type(sem, expr_token);
			}
			
			if (ee_type_is_float(right))
			{
				const Token* expr_token = ee_expr_get_token(expr->as_binop.right);

				ee_log_error_token(&sem->log, expr_token,
					"Invalid operand type (%s) for operation (%s)",
					_s_dtype_names[right->as_primitive.dtype], _s_op_binop_name_table[expr->as_binop.type]);

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

				ee_log_error_token(&sem->log, expr_token,
					"Logical operator (%s) require boolean operands, got (%s) and (%s)",
					_s_op_binop_name_table[expr->as_binop.type], _s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return &sem->builtin_types[DTYPE_BOOL];
		} break;
		case BINOP_CAST:
		{
			if (!ee_sem_type_can_cast(left, right))
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Invalid left hand side type (%s) for cast into (%s)",
					_s_dtype_names[left->as_primitive.dtype], _s_dtype_names[right->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}
			
			return right;
		}
		default: EE_ASSERT(0, "Unknown operator type (%d)", expr->as_binop.type);
		}
	} break;
	case EXPR_UNOP:
	{
		Sem_Type* op_type = ee_sem_get_expr_type(sem, expr->as_unop.expr, scope);

		if (op_type->type == TYPE_ERROR)
			return op_type;

		switch (expr->as_unop.type)
		{
		case UNOP_NOT:
		{
			if (!ee_type_is_bool(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Logical NOT '!' operator requires a boolean operand, got (%s)",
					_s_dtype_names[op_type->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return &sem->builtin_types[DTYPE_BOOL];
		} break;

		case UNOP_PLUS:
		case UNOP_MINUS:
		{
			if (!ee_type_is_int(op_type) && !ee_type_is_uint(op_type) && !ee_type_is_float(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Unary operator (%d) require a numeric operand, got (%s)",
					_s_op_unop_name_table[expr->as_unop.type], _s_dtype_names[op_type->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return op_type;
		} break;

		case UNOP_BW_NOT:
		{
			if (!ee_type_is_int(op_type) && !ee_type_is_uint(op_type))
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Bitwise NOT '~' operator requires an integer operand, got (%d)",
					op_type->type);

				return ee_sem_create_error_type(sem, expr_token);
			}

			return op_type;
		} break;

		case UNOP_PTR:
		{
			Sem_Type* ptr_type = ee_sem_alloc_type(sem, TYPE_PTR);

			ptr_type->as_ptr.to = op_type;
			ptr_type->size = 8;
			ptr_type->align = 8;

			return ptr_type;
		} break;

		case UNOP_DEREF:
		{
			if (op_type->type != TYPE_PTR)
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Dereference '*' operator requires a pointer operand, got (%d)",
					op_type->as_primitive.dtype);

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

		if (func->type == TYPE_ERROR)
			return func;

		if (func->type != TYPE_FUNC)
		{
			const Token* expr_token = ee_expr_get_token(expr);

			ee_log_error_token(&sem->log, expr_token,
				"Trying to call non-functional type (%d)",
				func->type);

			return ee_sem_create_error_type(sem, expr_token);
		}

		if (ee_array_len(&expr->as_func_call.args) != ee_array_len(&func->as_func.params))
		{
			const Token* expr_token = ee_expr_get_token(expr);

			ee_log_error_token(&sem->log, expr_token,
				"Expected (%zu) args, but (%zu) were given",
				ee_array_len(&func->as_func.params), ee_array_len(&expr->as_func_call.args));

			return ee_sem_create_error_type(sem, expr_token);
		}

		const Ast_Expr** args = (const Ast_Expr**)expr->as_func_call.args.buffer;
		Sem_Type** params = (Sem_Type**)func->as_func.params.buffer;

		for (size_t i = 0; i < ee_array_len(&func->as_func.params); ++i)
		{
			Sem_Type* arg = ee_sem_get_expr_type(sem, args[i], scope);
			Sem_Type* param = params[i];

			if (arg->type == TYPE_ERROR)
				return arg;

			if (!ee_types_match(arg, param)) // TODO(eesuck): parameter type can not expand
			{
				const Token* expr_token = ee_expr_get_token(expr);

				ee_log_error_token(&sem->log, expr_token,
					"Type mismatch for argument (%zu): expected (%s), got (%s)",
					i, _s_dtype_names[param->as_primitive.dtype], _s_dtype_names[arg->as_primitive.dtype]);

				return ee_sem_create_error_type(sem, expr_token);
			}
		}

		return func->as_func.ret;
	} break;
	default: EE_ASSERT(0, "Unknown expression type (%d)", expr->type);
	}
}

Sem_Type* ee_sem_resolve_type(Sem_Analyzer* sem, Ast_Type* ast_type, Sem_Scope* scope)
{
	if (ast_type == NULL)
	{
		return &sem->builtin_types[DTYPE_VOID];
	}

	switch (ast_type->type)
	{
	case TYPE_PRIMITIVE:
	{
		Sem_Type* out = ee_scope_lookup_type(sem, scope, ast_type->as_primitive.ident);

		if (out != NULL)
			return out;

		ee_sem_check_or_panic(sem, EE_FALSE, ast_type->as_primitive.ident, "Unknown type name");

		return ee_sem_create_error_type(sem, ast_type->as_primitive.ident);
	} break;
	case TYPE_STRUCT:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, TYPE_STRUCT);
		out->as_struct.members = ee_array_new(8, sizeof(Sem_Struct_Member), &sem->allocator);

		Ast_Type** members = (Ast_Type**)ast_type->as_struct.members.buffer;
		Usize offset = 0;

		out->align = 1;

		for (size_t i = 0; i < ee_array_len(&ast_type->as_struct.members); ++i)
		{
			Sem_Type* member = ee_sem_resolve_type(sem, members[i], scope);
			Sem_Struct_Member struct_member = { 0 };

			if (member->type == TYPE_ERROR)
			{
				continue;
			}

			struct_member.ident = NULL;
			struct_member.type = member;

			ee_array_push(&out->as_struct.members, EE_RECAST_U8(struct_member));

			if (member->align > out->align)
				out->align = member->align;

			offset = ee_round_up_pow2(offset, member->align);
			struct_member.offset = offset;
			offset += member->size;
		}

		out->token = &_s_anonymous_token;
		out->size = ee_round_up_pow2(offset, out->align);

		return out;
	} break;
	case TYPE_PTR:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, TYPE_PTR);

		out->token = &_s_anonymous_token;
		out->size = 8;
		out->align = 8;
		out->as_ptr.to = ee_sem_resolve_type(sem, ast_type->as_ptr.to, scope);

		return out;
	} break;
	case TYPE_FUNC:
	{
		Sem_Type* out = ee_sem_alloc_type(sem, TYPE_FUNC);
		Ast_Type** params = (Ast_Type**)ast_type->as_func.params.buffer;

		out->as_func.params = ee_array_new(8, sizeof(Sem_Type*), &sem->allocator);

		for (size_t i = 0; i < ee_array_len(&ast_type->as_func.params); ++i)
		{
			Sem_Type* param = ee_sem_resolve_type(sem, params[i], scope);
			ee_array_push(&out->as_func.params, EE_RECAST_U8(param));
		}
		
		out->token = &_s_anonymous_token;
		out->size = 8;
		out->align = 8;

		if (ast_type->as_func.ret != NULL)
			out->as_func.ret = ee_sem_resolve_type(sem, ast_type->as_func.ret, scope);

		return out;
	} break;
	default: EE_ASSERT(0, "Trying to resolve unknown type (%d)", ast_type->type);
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

		Sem_Type* func_type = ee_sem_resolve_type(sem, stmt->as_func_decl.type_info, scope);
		Sem_Entry* func_entry = ee_alloc_entry(sem, SEM_ENTRY_FUNC, stmt->as_func_decl.ident, stmt, func_type, (Sem_Value) { 0 });

		ee_sem_check_or_panic(sem, ee_scope_lookup_entry(scope, stmt->as_func_decl.ident) == NULL, stmt->as_func_decl.ident,
			"Trying to redifine an existing function");

		ee_scope_set_entry(scope, func_entry->ident, func_entry);
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

			if (value_type->type == TYPE_ERROR)
				return;
		}

		if (stmt->as_var_decl.type_info != NULL)
		{
			declared_type = ee_sem_resolve_type(sem, stmt->as_var_decl.type_info, parent);

			if (declared_type->type == TYPE_ERROR)
				return;
		}

		if (declared_type != NULL && value_type != NULL)
		{
			if (!ee_sem_type_can_cast(declared_type, value_type))
			{
				const Token* expr_token = ee_expr_get_token(stmt->as_var_decl.val);

				ee_log_error_token(&sem->log, expr_token,
					"Type mismatch: cannot assign value of type (%s) to variable declared as (%s)",
					_s_dtype_names[value_type->as_primitive.dtype], _s_dtype_names[declared_type->as_primitive.dtype]);

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

		ee_scope_set_entry(parent, var_entry->ident, var_entry);
	} break;
	case STMT_FN:
	{
		ee_sem_check_or_panic(sem, ee_scope_lookup_entry(parent, stmt->as_func_decl.ident) != NULL, stmt->as_func_decl.ident,
			"Error with function header resolver");

		const Token** params = (const Token**)stmt->as_func_decl.params.buffer;
		Ast_Type** param_ast_types = (Ast_Type**)stmt->as_func_decl.type_info->as_func.params.buffer;

		Sem_Scope* func_scope = ee_alloc_scope(sem, parent);

		for (size_t i = 0; i < ee_array_len(&stmt->as_func_decl.params); ++i)
		{
			Ast_Type* ast_type = param_ast_types[i];
			Sem_Type* sem_type = ee_sem_resolve_type(sem, ast_type, parent);

			Sem_Entry* param_entry = ee_alloc_entry(sem, SEM_ENTRY_VAR, params[i], stmt, sem_type, (Sem_Value) { 0 });
			ee_scope_set_entry(func_scope, param_entry->ident, param_entry);
		}

		Sem_Entry* func_entry = ee_scope_lookup_entry(parent, stmt->as_func_decl.ident);
		ee_sem_check_or_panic(sem, func_entry != NULL, stmt->as_func_decl.ident, "Unresolved function entry");

		Sem_Type* func_type = func_entry->type_info;

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

		if (cond_type->type != TYPE_ERROR && !ee_type_is_bool(cond_type))
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_if.cond);

			ee_log_error_token(&sem->log, expr_token, 
				"Invalid condition expression type (%d), expected bool", cond_type->as_primitive.dtype);
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

		if (range_type->type == TYPE_ERROR)
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

		ee_scope_set_entry(for_scope, stmt->as_for.it, it_entry);
		ee_sem_resolve_stmt(sem, stmt->as_for.body, for_scope, func_ret_type);
	} break;
	case STMT_WHILE:
	{
		Sem_Type* cond_type = ee_sem_get_expr_type(sem, stmt->as_while.cond, parent);

		if (cond_type->type != TYPE_ERROR && !ee_type_is_bool(cond_type))
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_while.cond);

			ee_log_error_token(&sem->log, expr_token,
				"Invalid condition expression type (%d), expected bool", cond_type->as_primitive.dtype);
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
				ee_sem_check_or_panic(&sem->log, 0, expr_token, "Trying to return without a function");
			}
			else
			{
				ee_log(LOG_LVL_ERROR, "Trying to return without a function");
			}
		}

		Sem_Type* ret_type = &sem->builtin_types[DTYPE_VOID];

		if (stmt->as_ret.val != NULL)
			ret_type = ee_sem_get_expr_type(sem, stmt->as_ret.val, parent);

		if (ret_type->type != TYPE_ERROR && !ee_types_match(ret_type, func_ret_type))
		{
			if (stmt->as_ret.val != NULL)
			{
				const Token* expr_token = ee_expr_get_token(stmt->as_ret.val);
				ee_sem_check_or_panic(sem, 0, expr_token, "Invalid return value type (%s), expected (%s)",
					_s_dtype_names[ret_type->as_primitive.dtype], _s_dtype_names[func_ret_type->as_primitive.dtype]);
			}
			else
			{
				ee_log(LOG_LVL_ERROR, "Invalid return value type (%s), expected (%s)",
					_s_dtype_names[ret_type->as_primitive.dtype], _s_dtype_names[func_ret_type->as_primitive.dtype]);
			}
		}
	} break;
	case STMT_ASSIGN:
	{
		// TODO(eesuck): handle structures arrays etc.
		Sem_Entry* val = ee_scope_lookup_entry(parent, stmt->as_assign.ident->as_ident.token);

		if (val == NULL || val->decl_stmt == NULL)
			ee_sem_check_or_panic(sem, 0, val->ident, "Unresolved variable name");
	
		Sem_Type* lhs_type = ee_sem_get_expr_type(sem, stmt->as_assign.ident, parent);
		Sem_Type* rhs_type = ee_sem_get_expr_type(sem, stmt->as_assign.val, parent);

		if (lhs_type->type == TYPE_ERROR || rhs_type->type == TYPE_ERROR)
		{
			break;
		}

		if (!ee_types_match(lhs_type, rhs_type))
		{
			const Token* expr_token = ee_expr_get_token(stmt->as_assign.val);
			ee_log_error_token(&sem->log, expr_token,
				"Type mismatch: cannot assign value of type (%s) to variable of type (%s). Implicit casting is not allowed on assignment.",
				_s_dtype_names[rhs_type->as_primitive.dtype],
				_s_dtype_names[lhs_type->as_primitive.dtype]);
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
	out->symbols = ee_dict_new(32, sizeof(const Token*), sizeof(Sem_Entry), config);
	out->types = ee_dict_new(32, sizeof(const Token*), sizeof(Sem_Type*), config);

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
		if (ee_dict_contains(&scope->symbols, EE_RECAST_U8(ident)))
			return (Sem_Entry*)ee_dict_at(&scope->symbols, EE_RECAST_U8(ident));

		scope = scope->parent;
	}

	return NULL;
}

Sem_Type* ee_scope_lookup_type(Sem_Analyzer* sem, Sem_Scope* scope, const Token* ident)
{
	if (ident == NULL || ident->type == TOKEN_INVALID)
		return NULL; 
	
	Sem_Type* builtin = ee_sem_match_builtin(sem, ident);
	
	if (builtin != NULL)
		return builtin;
	
	while (scope)
	{
		if (ee_dict_contains(&scope->types, EE_RECAST_U8(ident)))
			return *(Sem_Type**)ee_dict_at(&scope->types, EE_RECAST_U8(ident));

		scope = scope->parent;
	}

	return NULL;
}

void ee_scope_set_entry(Sem_Scope* scope, const Token* ident, Sem_Entry* entry)
{
	EE_ASSERT(ident != NULL, "Trying to set NULL identifier");

	ee_dict_set(&scope->symbols, EE_RECAST_U8(ident), (const u8*)entry);
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

	memset(out.builtin_types, 0, sizeof(out.builtin_types));

	for (Data_Type dtype = 0; dtype < DTYPE_COUNT; ++dtype)
	{
		out.builtin_types[dtype].type  = TYPE_PRIMITIVE;
		out.builtin_types[dtype].size  = _s_dtype_sizes[dtype];
		out.builtin_types[dtype].align = _s_dtype_aligns[dtype];
		out.builtin_types[dtype].token = &_s_dtype_tokens[dtype];
		out.builtin_types[dtype].as_primitive.dtype = dtype;
	}

	return out;
}
