#include "ee_parser.h"

Parser ee_pars_new(const Array* tokens, const Allocator* allocator)
{
	Parser out = { 0 };

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

	out.tokens = tokens;
	out.nodes = ee_array_new(ee_array_len(tokens), sizeof(Ast_Node*), allocator);
	out.pos = 0;

	return out;
}

Ast_Type_Info* ee_pars_type_info(Parser* pars)
{
	Ast_Type_Info* type_info = pars->allocator.alloc_fn(&pars->allocator, sizeof(*type_info));
	EE_ASSERT(type_info != NULL, "Unable to allocate memory");

	if (ee_pars_check(pars, TOKEN_IDENTIFIER))
	{
		const Token* token = ee_pars_eat(pars);

		type_info->type = TYPE_FLAT;
		type_info->as_flat = token;

		//for (Ast_Data_Type dtype = DTYPE_U8; dtype < DTYPE_COUNT; ++dtype)
		//{
		//	if (memcmp(token->scratch.buffer, _s_type_name_table[dtype], _s_type_name_len_table[dtype]) == 0)
		//	{
		//		type_info->as_flat = dtype;
		//		break;
		//	}
		//}
	}
	else if (ee_pars_match(pars, '('))
	{
		type_info->type = TYPE_TUPLE;
		type_info->as_tuple = ee_array_new(8, sizeof(type_info), &pars->allocator);

		while (!ee_pars_match(pars, ')'))
		{
			Ast_Type_Info* member = ee_pars_type_info(pars);

			ee_array_push(&type_info->as_tuple, EE_RECAST_U8(member));

			if (ee_pars_match(pars, ')'))
				break;

			ee_pars_match_or_panic(pars, ',', "Expected comma as separator between type expression members");
		}
	}
	else if (ee_pars_match(pars, '&'))
	{
		type_info->type = TYPE_PTR;
		type_info->as_ptr_to = ee_pars_type_info(pars);
	}
	else if (ee_pars_match(pars, TOKEN_AND))
	{
		Ast_Type_Info* inner_ptr = pars->allocator.alloc_fn(&pars->allocator, sizeof(*inner_ptr));
		EE_ASSERT(inner_ptr != NULL, "Unable to allocate memory");

		inner_ptr->type = TYPE_PTR;
		inner_ptr->as_ptr_to = ee_pars_type_info(pars);

		type_info->type = TYPE_PTR;
		type_info->as_ptr_to = inner_ptr;
	}
	else
	{
		EE_ASSERT(0, "Invalid token for type expression (%d)", ee_pars_peek(pars)->type);

		return NULL;
	}

	return type_info;
}

Ast_Expr* ee_pars_atom(Parser* pars)
{
	Ast_Expr* atom = pars->allocator.alloc_fn(&pars->allocator, sizeof(*atom));
	EE_ASSERT(atom != NULL, "Unable to allocate memory");

	const Token* token = ee_pars_eat(pars);

	if (ee_token_is_lit(token))
	{
		atom->type = EXPR_LIT;
		atom->as_lit.token = token;
	}
	else if (token->type == TOKEN_IDENTIFIER)
	{
		atom->type = EXPR_IDENT;
		atom->as_ident.token = token;
	}
	else if (token->type == '(')
	{
		atom = ee_pars_expr(pars);
		ee_pars_match_or_panic(pars, ')', "Unmatched paren in group expression");
	}
	else if (ee_token_match_unop(token) != UNOP_COUNT)
	{
		atom->type = EXPR_UNOP;
		atom->as_unop.type = ee_token_match_unop(token);
		atom->as_unop.expr = ee_pars_atom(pars);
	}

	atom = ee_pars_postfix(pars, atom);

	return atom;
}

Ast_Expr* ee_pars_postfix(Parser* pars, Ast_Expr* atom)
{
	while (EE_TRUE)
	{
		if (ee_pars_match(pars, '('))
		{
			Ast_Expr* func_call = pars->allocator.alloc_fn(&pars->allocator, sizeof(*func_call));
			EE_ASSERT(func_call != NULL, "Unable to allocate memory");

			func_call->type = EXPR_FUNC_CALL;
			func_call->as_func_call.args = ee_array_new(8, sizeof(Ast_Expr*), &pars->allocator);
			func_call->as_func_call.func = atom;

			while (!ee_pars_match(pars, ')'))
			{
				Ast_Expr* member = ee_pars_expr(pars);

				ee_array_push(&func_call->as_func_call.args, EE_RECAST_U8(member));

				if (ee_pars_match(pars, ')'))
					break;

				ee_pars_match_or_panic(pars, ',', "Expected comma as separator between function call arguments");

				EE_ASSERT(!ee_pars_match(pars, ')'), "Trailing comma in function call");
			}

			atom = func_call;
		}
		else if (ee_pars_match(pars, '.'))
		{
			Ast_Expr* access_expr = pars->allocator.alloc_fn(&pars->allocator, sizeof(*access_expr));
			EE_ASSERT(access_expr != NULL, "Unable to allocate memory");

			ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Invalid access expression, expected identifier");

			access_expr->type = EXPR_ACCESS;
			access_expr->as_access.entity = atom;
			access_expr->as_access.member = ee_pars_eat(pars);

			atom = access_expr;
		}
		else if (ee_pars_match(pars, '['))
		{
			Ast_Expr* index_expr = pars->allocator.alloc_fn(&pars->allocator, sizeof(*index_expr));
			EE_ASSERT(index_expr != NULL, "Unable to allocate memory");

			index_expr->type = EXPR_INDEX;
			index_expr->as_index.entity = atom;
			index_expr->as_index.index = ee_pars_expr(pars);

			ee_pars_match_or_panic(pars, ']', "Unmatched square braket in index expression");

			atom = index_expr;
		}
		else
		{
			break;
		}
	}

	return atom;
}

Ast_Expr* ee_pars_expr_1(Parser* pars, Ast_Expr* lhs, Ast_Precedence min_prec)
{
	while (EE_TRUE)
	{
		const Token* token = ee_pars_peek(pars);
		Ast_Binop_Type binop = ee_token_match_binop(token);
		Ast_Precedence prec = ee_op_prec(binop);

		if (binop == BINOP_COUNT || prec < min_prec)
			break;

		ee_pars_advance(pars, 1);

		Ast_Expr* rhs = ee_pars_atom(pars);

		const Token* next = ee_pars_peek(pars);
		Ast_Binop_Type next_op = ee_token_match_binop(next);
		Ast_Precedence next_prec = ee_op_prec(next_op);

		if (next_op != BINOP_COUNT && next_prec > prec)
		{
			rhs = ee_pars_expr_1(pars, rhs, prec + 1);
		}

		Ast_Expr* expr = pars->allocator.alloc_fn(&pars->allocator, sizeof(*expr));
		EE_ASSERT(expr != NULL, "Unable to allocate memory");

		expr->type = EXPR_BINOP;
		expr->as_binop.type = binop;
		expr->as_binop.left = lhs;
		expr->as_binop.right = rhs;

		lhs = expr;
	}

	return lhs;
}

Ast_Expr* ee_pars_expr(Parser* pars)
{
	Ast_Expr* lhs = ee_pars_atom(pars);

	return ee_pars_expr_1(pars, lhs, EE_EXPR_PREC_MIN);
}

void ee_pars_debug_print_type_info(Ast_Type_Info* root, size_t indent)
{
	EE_ASSERT(root != NULL, "Trying to print NULL type info root");

	if (root->type == TYPE_FLAT)
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		for (size_t i = 0; i < root->as_flat->scratch.len; ++i)
		{
			EE_PRINT("%c", root->as_flat->scratch.buffer[i]);
		}
		EE_PRINTLN("");
	}
	else if (root->type == TYPE_TUPLE)
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("TYPE_TUPLE: ");

		Ast_Type_Info** members = (Ast_Type_Info**)root->as_tuple.buffer;

		for (size_t i = 0; i < ee_array_len(&root->as_tuple); ++i)
		{
			ee_pars_debug_print_type_info(members[i], indent + 1);
		}
	}
	else if (root->type == TYPE_PTR)
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("TYPE_PTR: ");
		ee_pars_debug_print_type_info(root->as_ptr_to, indent);
	}
}

void ee_pars_debug_print_expr(Ast_Expr* expr, size_t indent)
{
	switch (expr->type)
	{
	case EXPR_LIT:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		if (expr->as_lit.token->type == TOKEN_LIT_INT)
		{
			EE_PRINTLN("LITERAL: %llu", expr->as_lit.token->as_u64);
		}
		else if (expr->as_lit.token->type == TOKEN_LIT_FLOAT)
		{
			EE_PRINTLN("LITERAL: %f", expr->as_lit.token->as_f64);
		}
		else
			EE_ASSERT(0, "Unsupported literal type for debug print (%d)", expr->as_lit.token->type);

		EE_PRINTLN("");
	} break;
	case EXPR_IDENT:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINT("IDENT: ");
		for (size_t i = 0; i < expr->as_ident.token->scratch.len; ++i)
		{
			EE_PRINT("%c", expr->as_ident.token->scratch.buffer[i]);
		}

		EE_PRINTLN("");
	} break;
	case EXPR_BINOP:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("BINOP: '%s'", _s_op_binop_name_table[expr->as_binop.type]);

		ee_pars_debug_print_expr(expr->as_binop.left, indent + 1);
		ee_pars_debug_print_expr(expr->as_binop.right, indent + 1);
	} break;
	case EXPR_UNOP:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("UNOP: '%s'", _s_op_unop_name_table[expr->as_unop.type]);

		ee_pars_debug_print_expr(expr->as_unop.expr, indent + 1);
	} break;
	case EXPR_FUNC_CALL:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("FUNC_CALL: ");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("FUNC_EXPR: ");

		ee_pars_debug_print_expr(expr->as_func_call.func, indent + 1);
		EE_PRINTLN("");

		Ast_Expr** args = (Ast_Expr**)expr->as_func_call.args.buffer;
		
		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("FUNC_ARGS: ");
		for (size_t i = 0; i < ee_array_len(&expr->as_func_call.args); ++i)
		{
			ee_pars_debug_print_expr(args[i], indent + 2);
		}
	} break;
	case EXPR_ACCESS:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("ACCESS: ");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("ACCESS_EXPR: ");
		ee_pars_debug_print_expr(expr->as_access.entity, indent + 1);

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINT("MEMBER: ");
		for (size_t i = 0; i < expr->as_access.member->scratch.len; ++i)
		{
			EE_PRINT("%c", expr->as_access.member->scratch.buffer[i]);
		}
		EE_PRINTLN("");
	} break;
	case EXPR_INDEX:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("INDEX: ");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("INDEX_EXPR: ");

		ee_pars_debug_print_expr(expr->as_index.entity, indent + 1);

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("INDEX_ARG: ");
		ee_pars_debug_print_expr(expr->as_index.index, indent + 2);
	} break;
	default:
	{
		EE_ASSERT(0, "Unknown expression type (%d)", expr->type);
	}
	}
}

void ee_pars_run(Parser* pars)
{
}