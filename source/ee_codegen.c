#include "ee_codegen.h"

Codegen ee_gen_new(const Sem_Scope* scope, const Ast_Module* mod, const char* file_out)
{
	Codegen out = { 0 };

	out.scope = scope;
	out.mod = mod;
	out.file_out = file_out;
	out.handle = NULL;

	return out;
}

void ee_gen_run(Codegen* gen)
{
	gen->handle = fopen(gen->file_out, "wb");
	EE_ASSERT(gen->handle != NULL, "Unable to create file at (%s)", gen->file_out);

	ee_gen_prologue(gen);
	ee_gen_stmt(gen, gen->mod->root, gen->scope);

	fclose(gen->handle);
}

void ee_gen_prologue(Codegen* gen)
{
	fwrite(_s_include_prol, 1, _s_include_prol_len, gen->handle);
	fwrite(_s_typedef_prol, 1, _s_typedef_prol_len, gen->handle);
}

void ee_gen_type(Codegen* gen, Sem_Type* type)
{
	switch (type->type)
	{
	case TYPE_PRIMITIVE:
	{
		fwrite(_s_dtype_names[type->as_primitive.dtype], 1,
			_s_dtype_lens_names[type->as_primitive.dtype], gen->handle);
	} break;
	case TYPE_PTR:
	{
		EE_ASSERT(type->as_ptr.to->type == TYPE_PRIMITIVE, "Only primitive type pointers supported now");

		ee_gen_type(gen, type->as_ptr.to);
		fputc('*', gen->handle);
	} break;
	default: EE_ASSERT(0, "Unknown type expression type (%d)", type->type);
	}
}

void ee_gen_expr(Codegen* gen, Ast_Expr* expr)
{
	switch (expr->type)
	{
	case EXPR_LIT:
	{
		const Token* token = expr->as_lit.token;
		fwrite(token->scratch.buffer, 1, token->scratch.len, gen->handle);
	} break;
	case EXPR_IDENT:
	{
		const Token* token = expr->as_ident.token;
		fwrite(token->scratch.buffer, 1, token->scratch.len, gen->handle);
	} break;
	case EXPR_BINOP:
	{
		if (expr->type == BINOP_CAST)
		{
			fputc('(', gen->handle);

			fputc('(', gen->handle);
			ee_gen_expr(gen, expr->as_binop.right);
			fputc(')', gen->handle);

			ee_gen_expr(gen, expr->as_binop.left);

			fputc(')', gen->handle);
		}
		else
		{
			fputc('(', gen->handle);

			ee_gen_expr(gen, expr->as_binop.left);
			fwrite(_s_op_binop_name_table[expr->as_binop.type], 1,
				_s_op_binop_name_len_table[expr->as_binop.type], gen->handle);
			ee_gen_expr(gen, expr->as_binop.right);

			fputc(')', gen->handle);
		}
	} break;
	case EXPR_UNOP:
	{
		fputc('(', gen->handle);

		fwrite(_s_op_unop_name_table[expr->as_unop.type], 1,
			_s_op_unop_name_len_table[expr->as_unop.type], gen->handle);
		ee_gen_expr(gen, expr->as_unop.expr);

		fputc(')', gen->handle);
	} break;
	case EXPR_FUNC_CALL:
	{
		Ast_Expr** args = (Ast_Expr**)expr->as_func_call.args.buffer;
		size_t argc = ee_array_len(&expr->as_func_call.args);

		fputc('(', gen->handle);
		
		fputc('(', gen->handle);
		ee_gen_expr(gen, expr->as_func_call.func);
		fputc(')', gen->handle);
		
		fputc('(', gen->handle);
		for (size_t i = 0; i < argc; ++i)
		{
			ee_gen_expr(gen, args[i]);
			
			if (i != argc - 1)
				fwrite(", ", 1, 2, gen->handle);
		}
		fputc(')', gen->handle);


		fputc(')', gen->handle);
	} break;
	default: EE_ASSERT(0, "Unknown expression type for codegen (%d)", expr->type);
	}
}

void ee_gen_stmt(Codegen* gen, Ast_Stmt* stmt, Sem_Scope* scope)
{
	switch (stmt->type)
	{
	case STMT_VAR_DECL:
	{
		Sem_Entry* var = ee_scope_lookup_entry(scope, stmt->as_var_decl.ident);

		ee_gen_type(gen, var->type_info);
		fputc(' ', gen->handle);
		fwrite(var->ident->scratch.buffer, 1, var->ident->scratch.len, gen->handle);

		if (stmt->as_var_decl.val != NULL)
		{
			fwrite(" = ", 1, 3, gen->handle);
			ee_gen_expr(gen, stmt->as_var_decl.val);
		}

		fwrite(";\n", 1, 2, gen->handle);
	} break;
	case STMT_BLOCK:
	{
		Sem_Scope* sub_scope = *(Sem_Scope**)ee_array_at(&scope->children, scope->top++);
		Bool is_global_block = (scope->parent == NULL);

		if (!is_global_block)
			fwrite("{\n", 1, 2, gen->handle);

		Ast_Stmt** stmts = (Ast_Stmt**)stmt->as_block.stmts.buffer;
		for (size_t i = 0; i < ee_array_len(&stmt->as_block.stmts); ++i)
		{
			ee_gen_stmt(gen, stmts[i], sub_scope);
		}

		if (!is_global_block)
			fwrite("}\n\n", 1, 3, gen->handle);
	} break;
	case STMT_FN:
	{
		Sem_Entry* func = ee_scope_lookup_entry(scope, stmt->as_func_decl.ident);
		const Token** params = (const Token**)stmt->as_func_decl.params.buffer;

		ee_gen_type(gen, func->type_info->as_func.ret);
		fputc(' ', gen->handle);
		fwrite(func->ident->scratch.buffer, 1, func->ident->scratch.len, gen->handle);
		
		Sem_Scope* params_scope = *(Sem_Scope**)ee_array_at(&scope->children, scope->top++);

		fputc('(', gen->handle);
		for (size_t i = 0; i < ee_array_len(&stmt->as_func_decl.params); ++i)
		{
			const Token* param = params[i];
			Sem_Entry* param_entry = ee_scope_lookup_entry(params_scope, param);

			ee_gen_type(gen, param_entry->type_info);
			fputc(' ', gen->handle);
			fwrite(param_entry->ident->scratch.buffer, 1, param_entry->ident->scratch.len, gen->handle);

			if (i != ee_array_len(&stmt->as_block.stmts) - 1)
			{
				fwrite(", ", 1, 2, gen->handle);
			}
		}
		fwrite(")\n", 1, 2, gen->handle);

		ee_gen_stmt(gen, stmt->as_func_decl.body, params_scope);
	} break;
	case STMT_RETURN:
	{
		fwrite("return ", 1, 7, gen->handle);
		ee_gen_expr(gen, stmt->as_ret.val);
		fwrite(";\n", 1, 2, gen->handle);
	} break;
	case STMT_EXPR:
	{
		ee_gen_expr(gen, stmt->as_expr.expr);
		fwrite(";\n", 1, 2, gen->handle);
	} break;
	case STMT_ASSIGN:
	{
		ee_gen_expr(gen, stmt->as_assign.ident);
		fwrite(" = ", 1, 3, gen->handle);
		ee_gen_expr(gen, stmt->as_assign.val);
		fwrite(";\n", 1, 2, gen->handle);
	} break;
	}
}
