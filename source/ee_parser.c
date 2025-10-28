#include "ee_parser.h"

static u64 ee_token_ptr_hash(const u8* key, size_t len)
{
	EE_UNUSED(len);
	const Token* token = *(const Token**)key;

	EE_ASSERT(token->type == TOKEN_IDENTIFIER, "Wrong key for type table hash function, expected identifier, got (%d)", token->type);

	return ee_hash_fast((const u8*)token->scratch.buffer, token->scratch.len);
}

static i32 ee_token_ptr_eq(const u8* a, const u8* b, size_t len)
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

	DictConfig config = ee_dict_config_new(&out.allocator, ee_token_ptr_hash, ee_token_ptr_eq, NULL, NULL);

	out.tokens = tokens;
	out.nodes = ee_array_new(ee_array_len(tokens) / 3, sizeof(Ast_Node*), allocator);
	out.var_types_table = ee_dict_new(ee_array_len(tokens) / 3, sizeof(const Token*), sizeof(Ast_Type_Info*), config);
	out.types_table = ee_dict_new(256, sizeof(const Token*), sizeof(Ast_Type_Info*), config);
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

Ast_Expr* ee_alloc_expr(Parser* pars, Ast_Expr_Type type)
{
	Ast_Expr* out = pars->allocator.alloc_fn(&pars->allocator, sizeof(*out));
	EE_ASSERT(out != NULL, "Unable to allocate memory");

	out->type = type;

	return out;
}

Ast_Stmt* ee_alloc_stmt(Parser* pars, Ast_Stmt_Type type)
{
	Ast_Stmt* stmt = pars->allocator.alloc_fn(&pars->allocator, sizeof(*stmt));
	EE_ASSERT(stmt != NULL, "Unable to allocate memory");

	stmt->type = type;

	return stmt;
}

Ast_Expr* ee_pars_atom(Parser* pars)
{
	Ast_Expr* atom = NULL;

	const Token* token = ee_pars_eat(pars);

	if (ee_token_is_lit(token))
	{
		atom = ee_alloc_expr(pars, EXPR_LIT);
		atom->as_lit.token = token;
	}
	else if (token->type == TOKEN_IDENTIFIER)
	{
		atom = ee_alloc_expr(pars, EXPR_IDENT);
		atom->as_ident.token = token;
	}
	else if (token->type == '(')
	{
		atom = ee_pars_expr(pars);
		ee_pars_match_or_panic(pars, ')', "Unmatched paren in group expression");
	}
	else if (ee_token_match_unop(token) != UNOP_COUNT)
	{
		atom = ee_alloc_expr(pars, EXPR_UNOP);
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
			Ast_Expr* func_call = ee_alloc_expr(pars, EXPR_FUNC_CALL);

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
			Ast_Expr* access_expr = ee_alloc_expr(pars, EXPR_ACCESS);

			ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Invalid access expression, expected identifier");

			access_expr->as_access.entity = atom;
			access_expr->as_access.member = ee_pars_eat(pars);

			atom = access_expr;
		}
		else if (ee_pars_match(pars, '['))
		{
			Ast_Expr* index_expr = ee_alloc_expr(pars, EXPR_INDEX);

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

		Ast_Expr* expr = ee_alloc_expr(pars, EXPR_BINOP);

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

Ast_Stmt* ee_pars_stmt(Parser* pars)
{
	Ast_Stmt* stmt = NULL;
	const Token* token = ee_pars_peek(pars);

	switch (token->type)
	{
	case TOKEN_LET:
	{
		ee_pars_advance(pars, 1);
		ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Expected identifier after 'let' keyword");

		const Token* ident = ee_pars_eat(pars);

		stmt = ee_alloc_stmt(pars, STMT_LET);
		stmt->as_let.ident = ident;

		if (ee_pars_match(pars, ':'))
		{
			Ast_Type_Info* type_info = ee_pars_type_info(pars);

			EE_ASSERT(type_info != NULL, "Invalid type information in variable declaration");

			ee_dict_set(&pars->var_types_table, EE_RECAST_U8(ident), EE_RECAST_U8(type_info));
			
			if (ee_pars_match(pars, '='))
				stmt->as_let.val = ee_pars_expr(pars);
			else
				stmt->as_let.val = NULL;

			ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression in variable declaration");
		}
		else if (ee_pars_match(pars, '='))
		{
			stmt->as_let.val = ee_pars_expr(pars);
			
			ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression in variable declaration");
		}
		else
		{
			EE_ASSERT(0, "Invalid token after identifier in variable declaration");
		}
	} break;
	case '{':
	{
		ee_pars_advance(pars, 1);
		
		stmt = ee_alloc_stmt(pars, STMT_BLOCK);
		stmt->as_block.stmts = ee_array_new(32, sizeof(Ast_Stmt*), &pars->allocator);

		while (!ee_pars_match(pars, '}'))
		{
			Ast_Stmt* block_stmt = ee_pars_stmt(pars);
			ee_array_push(&stmt->as_block.stmts, EE_RECAST_U8(block_stmt));
		}
	} break;
	case TOKEN_IF:
	{
		ee_pars_advance(pars, 1);

		EE_ASSERT(!ee_pars_check(pars, '{'), "Invalid 'if' statement, condition missed");
		
		stmt = ee_alloc_stmt(pars, STMT_IF);
		stmt->as_if.cond = ee_pars_expr(pars);

		ee_pars_check_or_panic(pars, '{', "Invalid 'if' statement, block body must be opened with '{'");
		stmt->as_if.if_block = ee_pars_stmt(pars);

		if (ee_pars_match(pars, TOKEN_ELSE))
		{
			ee_pars_check_or_panic(pars, '{', "Invalid 'if' statement, 'else' block body must be opened with '{'");
			stmt->as_if.else_block = ee_pars_stmt(pars);
		}
		else
		{
			stmt->as_if.else_block = NULL;
		}
	} break;
	case TOKEN_FOR:
	{
		ee_pars_advance(pars, 1);
		EE_ASSERT(ee_pars_check(pars, TOKEN_IDENTIFIER), "Invalid 'for' statement, expected iteration identifier after 'for' keyword");

		const Token* it = ee_pars_eat(pars);

		ee_pars_match_or_panic(pars, TOKEN_IN, "Invalid 'for' statement, expected 'in' keyword after iteration identifier");

		stmt = ee_alloc_stmt(pars, STMT_FOR);
		stmt->as_for.it = it;
		stmt->as_for.range = ee_pars_expr(pars);

		EE_ASSERT(stmt->as_for.range->type == EXPR_BINOP && stmt->as_for.range->as_binop.type == BINOP_RANGE,
			"Invalid 'for' statement, expected range '..' expression after 'in' keyword");

		stmt->as_for.body = ee_pars_stmt(pars);
	} break;
	case TOKEN_WHILE:
	{
		ee_pars_advance(pars, 1);

		EE_ASSERT(!ee_pars_check(pars, '{'), "Invalid 'while' statement, condition missed");

		stmt = ee_alloc_stmt(pars, STMT_WHILE);
		stmt->as_while.cond = ee_pars_expr(pars);

		ee_pars_check_or_panic(pars, '{', "Invalid 'if' statement, block body must be opened with '{'");
		stmt->as_while.body = ee_pars_stmt(pars);
	} break;
	default:
	{
		Ast_Expr* expr = ee_pars_expr(pars);

		if (ee_pars_match(pars, '='))
		{
			stmt = ee_alloc_stmt(pars, STMT_ASSIGN);
			stmt->as_assign.ident = expr;
			stmt->as_assign.val = ee_pars_expr(pars);
		}
		else
		{
			stmt = ee_alloc_stmt(pars, STMT_EXPR);
			stmt->as_expr.expr = expr;
		}

		ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression");
	} break;
	}

	return stmt;
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

		ee_str_view_print(root->as_flat->scratch);
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
		else if (expr->as_lit.token->type == TOKEN_LIT_STR)
		{
			EE_PRINT("LITERAL: \"");
			ee_str_view_print(expr->as_lit.token->as_str_view);
			EE_PRINT("\"\n");
		}
		else
			EE_ASSERT(0, "Unsupported literal type for debug print (%d)", expr->as_lit.token->type);
	} break;
	case EXPR_IDENT:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINT("IDENT: ");
		ee_str_view_print(expr->as_ident.token->scratch);
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

void ee_pars_debug_print_stmt(Ast_Stmt* stmt, size_t indent)
{
	switch (stmt->type)
	{
	case STMT_LET:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINT("LET: ");
		ee_str_view_print(stmt->as_let.ident->scratch);
		EE_PRINTLN("");
		
		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("LET_VAL: ");
		if (stmt->as_let.val != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_let.val, indent + 1);
		}
		else
		{
			for (size_t i = 0; i < indent + 1; ++i)
			{
				EE_PRINT("  ");
			}
			EE_PRINTLN("VAL_UNINIT");
		}
	} break;
	case STMT_BLOCK:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("BLOCK: ");

		Ast_Stmt** block_stmt = (Ast_Stmt**)stmt->as_block.stmts.buffer;

		for (size_t i = 0; i < ee_array_len(&stmt->as_block.stmts); ++i)
		{
			ee_pars_debug_print_stmt(block_stmt[i], indent + 1);
		}
	} break;
	case STMT_ASSIGN:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("ASSIGN: ");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("ASSIGN_LVAL:");

		ee_pars_debug_print_expr(stmt->as_assign.ident, indent + 1);

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("ASSIGN_RVAL: ");
		if (stmt->as_assign.val != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_assign.val, indent + 1);
		}
		else
		{
			for (size_t i = 0; i < indent + 1; ++i)
			{
				EE_PRINT("  ");
			}
			EE_PRINTLN("VAL_UNINIT");
		}
	} break;
	case STMT_EXPR:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("STMT_EXPR: ");
		ee_pars_debug_print_expr(stmt->as_expr.expr, indent + 1);

	} break;
	case STMT_IF:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("IF:");
		
		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("CONDITION:");

		ee_pars_debug_print_expr(stmt->as_if.cond, indent + 2);
		
		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("IF_BLOCK:");

		ee_pars_debug_print_stmt(stmt->as_if.if_block, indent + 2);

		if (stmt->as_if.else_block != NULL)
		{
			for (size_t i = 0; i < indent + 1; ++i)
			{
				EE_PRINT("  ");
			}
			EE_PRINTLN("ELSE_BLOCK:");

			ee_pars_debug_print_stmt(stmt->as_if.else_block, indent + 2);
		}
	} break;
	case STMT_FOR:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("FOR:");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINT("IT: ");
		ee_str_view_print(stmt->as_for.it->scratch);
		EE_PRINTLN("");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("RANGE:");
		ee_pars_debug_print_expr(stmt->as_for.range, indent + 1);

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("BODY:");
		ee_pars_debug_print_stmt(stmt->as_for.body, indent + 1);
	} break;
	case STMT_WHILE:
	{
		for (size_t i = 0; i < indent; ++i)
		{
			EE_PRINT("  ");
		}

		EE_PRINTLN("WHILE:");

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("CONDITION: ");
		ee_pars_debug_print_expr(stmt->as_while.cond, indent + 1);

		for (size_t i = 0; i < indent + 1; ++i)
		{
			EE_PRINT("  ");
		}
		EE_PRINTLN("BODY: ");
		ee_pars_debug_print_stmt(stmt->as_while.body, indent + 2);
	} break;
	default:
	{
		EE_ASSERT(0, "Invalid statement type for print (%d)", stmt->type);
	} break;
	}
}

void ee_pars_run(Parser* pars)
{
}