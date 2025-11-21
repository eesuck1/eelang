#include "ee_parser.h"

Parser ee_pars_new(const Lexer* lex, Logger log, const Allocator* allocator)
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

	out.tokens = &lex->tokens;
	out.pos = 0;
	out.logger = log;

	return out;
}

Ast_Expr* ee_alloc_expr(Parser* pars, Ast_Expr_Type type)
{
	Ast_Expr* out = pars->allocator.alloc_fn(&pars->allocator, sizeof(*out));
	EE_ASSERT(out != NULL, "Unable to allocate memory");

	memset(out, 0, sizeof(*out));
	out->type = type;

	return out;
}

Ast_Stmt* ee_alloc_stmt(Parser* pars, Ast_Stmt_Type type)
{
	Ast_Stmt* stmt = pars->allocator.alloc_fn(&pars->allocator, sizeof(*stmt));
	EE_ASSERT(stmt != NULL, "Unable to allocate memory");

	memset(stmt, 0, sizeof(*stmt));
	stmt->type = type;

	return stmt;
}

Ast_Expr* ee_pars_primary(Parser* pars)
{
	Ast_Expr* atom = NULL;
	const Token* token = ee_pars_peek(pars);

	switch (token->type)
	{
	case TOKEN_COMPTIME:
	{
		ee_pars_advance(pars, 1);

		atom = ee_pars_expr(pars);
		ee_expr_set_flag(atom, AST_FLAG_COMPTIME);
	} break;
	case TOKEN_LIT_FLOAT:
	case TOKEN_LIT_INT:
	case TOKEN_LIT_STR:
	case TOKEN_TRUE:
	case TOKEN_FALSE:
	{
		atom = ee_alloc_expr(pars, EXPR_LIT);
		atom->as_lit.token = ee_pars_eat(pars);
	} break;
	case TOKEN_IDENTIFIER:
	{
		atom = ee_alloc_expr(pars, EXPR_IDENT);
		atom->as_ident.token = ee_pars_eat(pars);
	} break;
	case '(':
	{
		ee_pars_advance(pars, 1);
		atom = ee_pars_expr(pars);
		ee_pars_match_or_panic(pars, ')', "Unmatched ')' in group expression");
	} break;
	case '[':
	{
		ee_pars_advance(pars, 1);
		atom = ee_alloc_expr(pars, EXPR_TYPE_ARRAY);
		atom->as_type_array.size = ee_pars_expr(pars);

		ee_pars_match_or_panic(pars, ']', "Expected ']' after array size");

		atom->as_type_array.type_expr = ee_pars_atom(pars);
	} break;
	case '.':
	{
		ee_pars_advance(pars, 1);
		ee_pars_match_or_panic(pars, '{', "Invalid token after '.', expected init list '{'");
		
		atom = ee_alloc_expr(pars, EXPR_INIT_LIST);

		atom->as_init_list.members = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Token*), &pars->allocator);
		atom->as_init_list.values = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Ast_Expr*), &pars->allocator);

		do
		{
			const Token* member = ee_pars_eat(pars);
			ee_pars_match_or_panic(pars, '=', "Expected '=' in init list");
			Ast_Expr* value = ee_pars_expr(pars);

			ee_linked_array_push(&atom->as_init_list.members, EE_RECAST_U8(member));
			ee_linked_array_push(&atom->as_init_list.values, EE_RECAST_U8(value));
		} while (ee_pars_match(pars, ','));

		ee_pars_match_or_panic(pars, '}', "Unmatched '}' in init list");
	} break;
	case TOKEN_STRUCT:
	{
		ee_pars_advance(pars, 1);

		atom = ee_alloc_expr(pars, EXPR_TYPE_STRUCT);

		atom->as_type_struct.members = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Token*), &pars->allocator);
		atom->as_type_struct.types = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Ast_Expr*), &pars->allocator);

		ee_pars_match_or_panic(pars, '{', "Expected '{' after 'struct' keyword");

		do
		{
			const Token* member = ee_pars_eat(pars);
			ee_pars_match_or_panic(pars, ':', "Expected ':' in struct member declaration");
			Ast_Expr* type = ee_pars_expr(pars);

			ee_linked_array_push(&atom->as_type_struct.members, EE_RECAST_U8(member));
			ee_linked_array_push(&atom->as_type_struct.types, EE_RECAST_U8(type));
		} while (ee_pars_match(pars, ','));

		ee_pars_match_or_panic(pars, '}', "Unmatched '}' in init list");
	} break;
	case TOKEN_UNION:
	{
		ee_pars_advance(pars, 1);

		atom = ee_alloc_expr(pars, EXPR_TYPE_UNION);

		atom->as_type_union.members = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Token*), &pars->allocator);
		atom->as_type_union.types = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Ast_Expr*), &pars->allocator);
		
		ee_pars_match_or_panic(pars, '{', "Expected '{' after 'union' keyword");

		do
		{
			const Token* member = ee_pars_eat(pars);
			ee_pars_match_or_panic(pars, ':', "Expected ':' in union member declaration");
			Ast_Expr* value = ee_pars_expr(pars);

			ee_linked_array_push(&atom->as_type_union.members, EE_RECAST_U8(member));
			ee_linked_array_push(&atom->as_type_union.types, EE_RECAST_U8(value));
		} while (ee_pars_match(pars, ','));

		ee_pars_match_or_panic(pars, '}', "Unmatched '}' in init list");
	} break;
	case TOKEN_ENUM:
	{
		ee_pars_advance(pars, 1);

		atom = ee_alloc_expr(pars, EXPR_TYPE_ENUM);

		atom->as_type_enum.members = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Token*), &pars->allocator);
		atom->as_type_enum.values = ee_linked_array_new(EE_TYPE_BASE_SIZE, sizeof(Ast_Expr*), &pars->allocator);
		
		ee_pars_match_or_panic(pars, '{', "Expected '{' after 'enum' keyword");

		do
		{
			const Token* member = ee_pars_eat(pars);
			ee_pars_match_or_panic(pars, '=', "Expected '=' in enum values declaration, implicit values does no supported");
			Ast_Expr* value = ee_pars_expr(pars);

			ee_linked_array_push(&atom->as_type_enum.members, EE_RECAST_U8(member));
			ee_linked_array_push(&atom->as_type_enum.values, EE_RECAST_U8(value));
		} while (ee_pars_match(pars, ','));

		ee_pars_match_or_panic(pars, '}', "Unmatched '}' in init list");
	} break;
	default:
	{
		ee_log_error_token(&pars->logger, token, "Unexpected token in expression");
		return NULL;
	} break;
	}

	return atom;
}

Ast_Expr* ee_pars_postfix(Parser* pars, Ast_Expr* atom)
{
	while (EE_TRUE)
	{
		if (ee_pars_match(pars, '('))
		{
			Ast_Expr* func_call = ee_alloc_expr(pars, EXPR_FUNC_CALL);

			func_call->as_func_call.args = ee_linked_array_new(EE_FUNC_ARGS_BASE_SIZE, sizeof(Ast_Expr*), &pars->allocator);
			func_call->as_func_call.func = atom;

			if (ee_pars_check(pars, ')'))
			{
				ee_pars_advance(pars, 1);
			}
			else
			{
				do
				{
					Ast_Expr* member = ee_pars_expr(pars);
					ee_linked_array_push(&func_call->as_func_call.args, EE_RECAST_U8(member));
				} while (ee_pars_match(pars, ','));

				ee_pars_match_or_panic(pars, ')', "Expected closing ')' at the end of arguments list");
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

Ast_Expr* ee_pars_atom(Parser* pars)
{
	Ast_Expr* atom = NULL;
	const Token* token = ee_pars_peek(pars);

	if (ee_token_match_unop(token) != UNOP_COUNT)
	{
		ee_pars_advance(pars, 1);

		atom = ee_alloc_expr(pars, EXPR_UNOP);
		atom->as_unop.type = ee_token_match_unop(token);
		atom->as_unop.expr = ee_pars_atom(pars);
	}
	else
	{
		atom = ee_pars_primary(pars);
	}

	return ee_pars_postfix(pars, atom);
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
	case TOKEN_COMPTIME:
	{
		ee_pars_advance(pars, 1);

		stmt = ee_pars_stmt(pars);
		ee_stmt_set_flag(stmt, AST_FLAG_COMPTIME);
	} break;
	case TOKEN_LET:
	{
		ee_pars_advance(pars, 1);
		ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Expected identifier after 'let' keyword");

		const Token* ident = ee_pars_eat(pars);

		stmt = ee_alloc_stmt(pars, STMT_VAR_DECL);
		stmt->as_var_decl.ident = ident;

		if (ee_pars_match(pars, ':'))
		{
			stmt->as_var_decl.type_expr = ee_pars_expr(pars);

			if (ee_pars_match(pars, '='))
				stmt->as_var_decl.val = ee_pars_expr(pars);
			else
				stmt->as_var_decl.val = NULL;

			ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression in variable declaration");
		}
		else if (ee_pars_match(pars, '='))
		{
			stmt->as_var_decl.val = ee_pars_expr(pars);

			ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression in variable declaration");
		}
		else
		{
			EE_ASSERT(0, "Invalid token after identifier in variable declaration");
		}
	} break;
	case TOKEN_CONST:
	{
		ee_pars_advance(pars, 1);
		ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Expected identifier after 'let' keyword");

		const Token* ident = ee_pars_eat(pars);

		stmt = ee_alloc_stmt(pars, STMT_VAR_DECL);
		stmt->as_var_decl.ident = ident;

		ee_stmt_set_flag(stmt, AST_FLAG_CONST);

		if (ee_pars_match(pars, ':'))
		{
			stmt->as_var_decl.type_expr = ee_pars_expr(pars);

			ee_pars_match_or_panic(pars, '=', "Constant value must be initialized");
			stmt->as_var_decl.val = ee_pars_expr(pars);

			ee_pars_match_or_panic(pars, ';', "Expected ';' after value expression in variable declaration");
		}
		else if (ee_pars_match_or_panic(pars, '=', "Constant value must be initialized"))
		{
			stmt->as_var_decl.val = ee_pars_expr(pars);

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
		stmt->as_block.stmts = ee_linked_array_new(EE_BLOCK_BASE_STMT_SIZE, sizeof(Ast_Stmt*), &pars->allocator);

		while (!ee_pars_match(pars, '}'))
		{
			Ast_Stmt* block_stmt = ee_pars_stmt(pars);
			ee_linked_array_push(&stmt->as_block.stmts, EE_RECAST_U8(block_stmt));
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
	case TOKEN_FUNCTION:
	{
		ee_pars_advance(pars, 1);
		ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Expected function name identifier after 'fn' keyword");

		const Token* ident = ee_pars_eat(pars);

		stmt = ee_alloc_stmt(pars, STMT_FN);
		stmt->as_func_decl.ident = ident;
		stmt->as_func_decl.params = ee_linked_array_new(EE_FUNC_ARGS_BASE_SIZE, sizeof(Ast_Func_Param), &pars->allocator);
		stmt->as_func_decl.ret_type = NULL;

		ee_pars_match_or_panic(pars, '(', "Expected '(' after function name");

		if (!ee_pars_check(pars, ')'))
		{
			do
			{
				Ast_Func_Param param = { 0 };
				Bool is_comptime = EE_FALSE;

				if (ee_pars_match(pars, TOKEN_COMPTIME))
					is_comptime = EE_TRUE;

				ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Expected function parameter name identifier in parameters list");
				param.ident = ee_pars_eat(pars);
				ee_pars_match_or_panic(pars, ':', "Expected ':' to separate parameter name and type expression");
				param.type = ee_pars_expr(pars);
				
				if (is_comptime)
					ee_expr_set_flag(param.type, AST_FLAG_COMPTIME);

				ee_linked_array_push(&stmt->as_func_decl.params, EE_RECAST_U8(param));
			} while (ee_pars_match(pars, ','));
		}

		ee_pars_match_or_panic(pars, ')', "Expected closed ')' after function parameters list");

		if (ee_pars_match(pars, TOKEN_ARROW))
		{
			stmt->as_func_decl.ret_type = ee_pars_expr(pars);
		}

		ee_pars_check_or_panic(pars, '{', "Expected '{' for function body");
		stmt->as_func_decl.body = ee_pars_stmt(pars);
	} break;
	case TOKEN_RETURN:
	{
		ee_pars_advance(pars, 1);

		stmt = ee_alloc_stmt(pars, STMT_RETURN);
		stmt->as_ret.val = NULL;

		if (!ee_pars_match(pars, ';'))
		{
			stmt->as_ret.val = ee_pars_expr(pars);
			ee_pars_match_or_panic(pars, ';', "Expected ';' after 'return' statement");
		}
	} break;
	case TOKEN_BREAK:
	{
		ee_pars_advance(pars, 1);
		stmt = ee_alloc_stmt(pars, STMT_BREAK);
		ee_pars_match_or_panic(pars, ';', "Expected ';' after 'break' statement");
	} break;
	case TOKEN_CONTINUE:
	{
		ee_pars_advance(pars, 1);
		stmt = ee_alloc_stmt(pars, STMT_CONTINUE);
		ee_pars_match_or_panic(pars, ';', "Expected ';' after 'continue' statement");
	} break;
	case TOKEN_DEFER:
	{
		ee_pars_advance(pars, 1);
		
		stmt = ee_alloc_stmt(pars, STMT_DEFER);
		stmt->as_defer.stmt = ee_pars_stmt(pars);
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

void ee_pars_debug_print_expr(Ast_Expr* expr, size_t indent)
{
	if (ee_expr_has_flag(expr, AST_FLAG_COMPTIME))
	{
		ee_print_indent(indent);
		EE_PRINTLN("COMPTIME ");
	}

	switch (expr->type)
	{
	case EXPR_LIT:
	{
		ee_print_indent(indent);

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
		else if (expr->as_lit.token->type == TOKEN_TRUE)
		{
			EE_PRINTLN("LITERAL: true");
		}
		else if (expr->as_lit.token->type == TOKEN_FALSE)
		{
			EE_PRINTLN("LITERAL: false");
		}
		else
			EE_ASSERT(0, "Unsupported literal type for debug print (%d)", expr->as_lit.token->type);
	} break;
	case EXPR_IDENT:
	{
		ee_print_indent(indent);
		EE_PRINT("IDENT: ");
		ee_str_view_print(expr->as_ident.token->scratch);
		EE_PRINTLN("");
	} break;
	case EXPR_BINOP:
	{
		ee_print_indent(indent);
		EE_PRINTLN("BINOP: '%s'", _s_op_binop_name_table[expr->as_binop.type]);

		ee_pars_debug_print_expr(expr->as_binop.left, indent + 1);
		ee_pars_debug_print_expr(expr->as_binop.right, indent + 1);
	} break;
	case EXPR_UNOP:
	{
		ee_print_indent(indent);
		EE_PRINTLN("UNOP: '%s'", _s_op_unop_name_table[expr->as_unop.type]);

		ee_pars_debug_print_expr(expr->as_unop.expr, indent + 1);
	} break;
	case EXPR_FUNC_CALL:
	{
		ee_print_indent(indent);
		EE_PRINTLN("FUNC_CALL: ");

		ee_print_indent(indent + 1);
		EE_PRINTLN("FUNC_EXPR: ");

		ee_pars_debug_print_expr(expr->as_func_call.func, indent + 1);
		EE_PRINTLN("");

		ee_print_indent(indent + 1);
		EE_PRINTLN("FUNC_ARGS: ");
		for (size_t i = 0; i < ee_linked_array_len(&expr->as_func_call.args); ++i)
		{
			Ast_Expr* arg = *(Ast_Expr**)ee_linked_array_at(&expr->as_func_call.args, i);

			ee_pars_debug_print_expr(arg, indent + 2);
		}
	} break;
	case EXPR_ACCESS:
	{
		ee_print_indent(indent);
		EE_PRINTLN("ACCESS: ");

		ee_print_indent(indent + 1);
		EE_PRINTLN("ACCESS_EXPR: ");

		ee_pars_debug_print_expr(expr->as_access.entity, indent + 1);
		ee_print_indent(indent + 1);

		EE_PRINT("MEMBER: ");
		for (size_t i = 0; i < expr->as_access.member->scratch.len; ++i)
		{
			EE_PRINT("%c", expr->as_access.member->scratch.buffer[i]);
		}
		EE_PRINTLN("");
	} break;
	case EXPR_INDEX:
	{
		ee_print_indent(indent);

		EE_PRINTLN("INDEX: ");

		ee_print_indent(indent + 1);
		EE_PRINTLN("INDEX_EXPR: ");

		ee_pars_debug_print_expr(expr->as_index.entity, indent + 1);
		ee_print_indent(indent + 1);

		EE_PRINTLN("INDEX_ARG: ");
		ee_pars_debug_print_expr(expr->as_index.index, indent + 2);
	} break;
	case EXPR_TYPE_STRUCT:
	{
		ee_print_indent(indent);
		EE_PRINTLN("TYPE_STRUCT:");

		for (size_t i = 0; i < ee_linked_array_len(&expr->as_type_struct.members); ++i)
		{
			Token* member = *(Token**)ee_linked_array_at(&expr->as_type_struct.members, i);
			Ast_Expr* type = *(Ast_Expr**)ee_linked_array_at(&expr->as_type_struct.types, i);

			ee_print_indent(indent + 1);
			EE_PRINT("MEMBER: ");
			ee_str_view_print(member->scratch);
			EE_PRINTLN("");
			ee_pars_debug_print_expr(type, indent + 2);
		}
	} break;
	case EXPR_TYPE_TUPLE:
	{
		ee_print_indent(indent);
		EE_PRINTLN("TYPE_TUPLE:");

		for (size_t i = 0; i < ee_linked_array_len(&expr->as_type_tuple.types); ++i)
		{
			Ast_Expr* type = *(Ast_Expr**)ee_linked_array_at(&expr->as_type_tuple.types, i);

			ee_pars_debug_print_expr(type, indent + 2);
		}
	} break;
	case EXPR_TYPE_UNION:
	{
		ee_print_indent(indent);
		EE_PRINTLN("TYPE_UNION:");

		for (size_t i = 0; i < ee_linked_array_len(&expr->as_type_union.types); ++i)
		{
			Ast_Expr* type = *(Ast_Expr**)ee_linked_array_at(&expr->as_type_tuple.types, i);
			
			ee_pars_debug_print_expr(type, indent + 2);
		}
	} break;
	case EXPR_TYPE_ARRAY:
	{
		ee_print_indent(indent);
		EE_PRINTLN("TYPE_ARRAY:");
		ee_print_indent(indent + 1);

		EE_PRINTLN("SIZE:");
		ee_pars_debug_print_expr(expr->as_type_array.size, indent + 2);
		ee_print_indent(indent + 1);

		EE_PRINTLN("TYPE:");
		ee_pars_debug_print_expr(expr->as_type_array.type_expr, indent + 2);
	} break;
	case EXPR_INIT_LIST:
	{
		ee_print_indent(indent);
		EE_PRINTLN("INIT_LIST:");

		for (size_t i = 0; i < ee_linked_array_len(&expr->as_init_list.members); ++i)
		{
			Token* member = *(Token**)ee_linked_array_at(&expr->as_init_list.members, i);
			Ast_Expr* value = *(Ast_Expr**)ee_linked_array_at(&expr->as_init_list.values, i);
			
			ee_print_indent(indent + 1);
			EE_PRINT("MEMBER: ");
			ee_str_view_print(member->scratch);
			EE_PRINTLN("");
			ee_pars_debug_print_expr(value, indent + 2);
		}
	} break;
	default:
	{
		EE_ASSERT(0, "Unknown expression type (%d)", expr->type);
	}
	}
}

void ee_pars_debug_print_stmt(Ast_Stmt* stmt, size_t indent)
{
	if (ee_stmt_has_flag(stmt, AST_FLAG_COMPTIME))
	{
		ee_print_indent(indent);
		EE_PRINTLN("COMPTIME ");
	}

	switch (stmt->type)
	{
	case STMT_VAR_DECL:
	{
		ee_print_indent(indent);

		if (ee_stmt_has_flag(stmt, AST_FLAG_CONST))
			EE_PRINT("COSNT: ");
		else
			EE_PRINT("LET: ");

		ee_str_view_print(stmt->as_var_decl.ident->scratch);
		EE_PRINTLN("");

		if (stmt->as_var_decl.type_expr != NULL)
		{
			ee_print_indent(indent + 1);
			EE_PRINTLN("LET_TYPE: ");
			ee_pars_debug_print_expr(stmt->as_var_decl.type_expr, indent + 2);
		}

		ee_print_indent(indent + 1);
		EE_PRINTLN("LET_VAL: ");
		if (stmt->as_var_decl.val != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_var_decl.val, indent + 2);
		}
		else
		{
			ee_print_indent(indent + 2);
			EE_PRINTLN("VAL_UNINIT");
		}
	} break;
	case STMT_BLOCK:
	{
		ee_print_indent(indent);
		EE_PRINTLN("BLOCK: ");

		for (size_t i = 0; i < ee_linked_array_len(&stmt->as_block.stmts); ++i)
		{
			Ast_Stmt* block_stmt = *(Ast_Stmt**)ee_linked_array_at(&stmt->as_block.stmts, i);
			
			ee_pars_debug_print_stmt(block_stmt, indent + 1);
		}
	} break;
	case STMT_ASSIGN:
	{
		ee_print_indent(indent);
		EE_PRINTLN("ASSIGN: ");

		ee_print_indent(indent + 1);
		EE_PRINTLN("ASSIGN_LVAL:");

		ee_pars_debug_print_expr(stmt->as_assign.ident, indent + 2);
		ee_print_indent(indent + 1);

		EE_PRINTLN("ASSIGN_RVAL: ");
		if (stmt->as_assign.val != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_assign.val, indent + 2);
		}
		else
		{
			ee_print_indent(indent + 2);
			EE_PRINTLN("VAL_UNINIT");
		}
	} break;
	case STMT_EXPR:
	{
		ee_print_indent(indent);

		EE_PRINTLN("STMT_EXPR: ");
		ee_pars_debug_print_expr(stmt->as_expr.expr, indent + 1);

	} break;
	case STMT_IF:
	{
		ee_print_indent(indent);

		EE_PRINTLN("IF:");

		ee_print_indent(indent + 1);
		EE_PRINTLN("CONDITION:");

		ee_pars_debug_print_expr(stmt->as_if.cond, indent + 2);

		ee_print_indent(indent + 1);
		EE_PRINTLN("IF_BLOCK:");

		ee_pars_debug_print_stmt(stmt->as_if.if_block, indent + 2);

		if (stmt->as_if.else_block != NULL)
		{
			ee_print_indent(indent + 1);
			EE_PRINTLN("ELSE_BLOCK:");

			ee_pars_debug_print_stmt(stmt->as_if.else_block, indent + 2);
		}
	} break;
	case STMT_FOR:
	{
		ee_print_indent(indent);

		EE_PRINTLN("FOR:");

		ee_print_indent(indent + 1);
		EE_PRINT("IT: ");
		ee_str_view_print(stmt->as_for.it->scratch);
		EE_PRINTLN("");

		ee_print_indent(indent + 1);
		EE_PRINTLN("RANGE:");
		ee_pars_debug_print_expr(stmt->as_for.range, indent + 2);

		ee_print_indent(indent + 1);
		EE_PRINTLN("BODY:");
		ee_pars_debug_print_stmt(stmt->as_for.body, indent + 2);
	} break;
	case STMT_WHILE:
	{
		ee_print_indent(indent);

		EE_PRINTLN("WHILE:");

		ee_print_indent(indent + 1);
		EE_PRINTLN("CONDITION: ");
		ee_pars_debug_print_expr(stmt->as_while.cond, indent + 2);

		ee_print_indent(indent + 1);
		EE_PRINTLN("BODY: ");
		ee_pars_debug_print_stmt(stmt->as_while.body, indent + 2);
	} break;
	case STMT_FN:
	{
		ee_print_indent(indent);

		EE_PRINT("FN: ");
		ee_str_view_print(stmt->as_func_decl.ident->scratch);
		EE_PRINTLN("");

		ee_print_indent(indent + 1);
		EE_PRINTLN("PARAMS: ");

		for (size_t i = 0; i < ee_linked_array_len(&stmt->as_func_decl.params); ++i)
		{
			Ast_Func_Param param = *(Ast_Func_Param*)ee_linked_array_at(&stmt->as_func_decl.params, i);
			
			ee_print_indent(indent + 2);
			ee_str_view_print(param.ident->scratch);
			EE_PRINTLN(":");
			ee_pars_debug_print_expr(param.type, indent + 3);
		}

		ee_print_indent(indent + 1);
		EE_PRINTLN("RETURN_TYPE: ");
		if (stmt->as_func_decl.ret_type != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_func_decl.ret_type, indent + 2);
		}
		else
		{
			ee_print_indent(indent + 2);
			EE_PRINTLN("VOID (implicit)");
		}

		ee_print_indent(indent + 1);
		EE_PRINTLN("BODY: ");
		ee_pars_debug_print_stmt(stmt->as_func_decl.body, indent + 2);
	} break;
	case STMT_RETURN:
	{
		ee_print_indent(indent);

		EE_PRINTLN("RETURN: ");

		if (stmt->as_ret.val != NULL)
		{
			ee_pars_debug_print_expr(stmt->as_ret.val, indent + 1);
		}
	} break;
	case STMT_BREAK:
	{
		ee_print_indent(indent);
		EE_PRINTLN("BREAK ");
	} break;
	case STMT_CONTINUE:
	{
		ee_print_indent(indent);
		EE_PRINTLN("CONTINUE ");
	} break;
	case STMT_DEFER:
	{
		ee_print_indent(indent);
		EE_PRINTLN("DEFER: ");
		ee_pars_debug_print_stmt(stmt->as_defer.stmt, indent + 1);
	} break;
	default:
	{
		EE_ASSERT(0, "Invalid statement type for print (%d)", stmt->type);
	} break;
	}
}

void ee_pars_debug_print_module(Ast_Module* mod)
{

	EE_PRINTLN("-- MODULE: '%s' --\n", mod->name);

	for (size_t i = 0; i < ee_linked_array_len(&mod->root->as_block.stmts); ++i)
	{
		Ast_Stmt* stmt = *(Ast_Stmt**)ee_linked_array_at(&mod->root->as_block.stmts, i);
		
		ee_pars_debug_print_stmt(stmt, 0);
		EE_PRINTLN("");
	}
}

Ast_Module* ee_pars_run(Parser* pars)
{
	Ast_Module* mod = pars->allocator.alloc_fn(&pars->allocator, sizeof(*mod));
	EE_ASSERT(mod != NULL, "Unable to allocate memory");

	mod->name = NULL;
	mod->root = ee_alloc_stmt(pars, STMT_BLOCK);
	mod->root->as_block.stmts = ee_linked_array_new(32, sizeof(Ast_Stmt*), &pars->allocator);

	while (!ee_pars_match(pars, TOKEN_EOF))
	{
		Ast_Stmt* stmt = ee_pars_stmt(pars);
		ee_linked_array_push(&mod->root->as_block.stmts, EE_RECAST_U8(stmt));
	}

	return mod;
}