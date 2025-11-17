#include "ee_codegen.h"

static const char _s_include_prol[] =
	"#include <stdint.h>\n"
	"#include <stdbool.h>\n"
	"#include <stddef.h>\n\n";

static const size_t _s_include_prol_len = sizeof(_s_include_prol) - 1;

static const char _s_typedef_prol[] =
	"typedef uint8_t     u8;\n"
	"typedef uint16_t    u16;\n"
	"typedef uint32_t    u32;\n"
	"typedef uint64_t    u64;\n"
	"typedef int8_t      i8;\n"
	"typedef int16_t     i16;\n"
	"typedef int32_t     i32;\n"
	"typedef int64_t     i64;\n"
	"typedef float       f32;\n"
	"typedef double      f64;\n"
	"typedef long double f80;\n\n";

static const size_t _s_typedef_prol_len = sizeof(_s_typedef_prol) - 1;

static void ee_gen_printf(Codegen* gen, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	vfprintf(gen->handle, fmt, args);

	va_end(args);
}

static void ee_gen_print_token(Codegen* gen, const Token* token)
{
	fprintf(gen->handle, "%.*s", (int)token->scratch.len, token->scratch.buffer);
}

static void ee_gen_print_indent(Codegen* gen, size_t indent)
{
	for (size_t i = 0; i < indent; ++i)
		fprintf(gen->handle, "    ");
}

const Token* ee_stmt_get_token(const Ast_Stmt* stmt)
{
	if (stmt == NULL) return NULL;

	switch (stmt->type)
	{
	case STMT_VAR_DECL: return stmt->as_var_decl.ident;
	case STMT_FN: return stmt->as_func_decl.ident;
	case STMT_BLOCK:
		if (ee_array_len(&stmt->as_block.stmts) > 0)
		{
			Ast_Stmt** stmts = (Ast_Stmt**)stmt->as_block.stmts.buffer;
			return ee_stmt_get_token(stmts[0]);
		}
		return NULL;
	case STMT_IF: return ee_expr_get_token(stmt->as_if.cond);
	case STMT_FOR: return stmt->as_for.it;
	case STMT_WHILE: return ee_expr_get_token(stmt->as_while.cond);
	case STMT_EXPR: return ee_expr_get_token(stmt->as_expr.expr);
	case STMT_RETURN:
		if (stmt->as_ret.val) return ee_expr_get_token(stmt->as_ret.val);
		return NULL;
	case STMT_ASSIGN: return ee_expr_get_token(stmt->as_assign.ident);
	}
	return NULL;
}

static const char* ee_gen_binop_to_c(Ast_Binop_Type type)
{
	switch (type)
	{
	case BINOP_PLUS: return "+";
	case BINOP_MINUS: return "-";
	case BINOP_MUL: return "*";
	case BINOP_DIV: return "/";
	case BINOP_MOD: return "%";
	case BINOP_EQUAL: return "==";
	case BINOP_NOT_EQUAL: return "!=";
	case BINOP_GREATER: return ">";
	case BINOP_GREATER_EQUAL: return ">=";
	case BINOP_LESS: return "<";
	case BINOP_LESS_EQUAL: return "<=";
	case BINOP_AND: return "&&";
	case BINOP_OR: return "||";
	case BINOP_BW_AND: return "&";
	case BINOP_BW_OR: return "|";
	case BINOP_BW_XOR: return "^";
	case BINOP_SHIFT_LEFT: return "<<";
	case BINOP_SHIFT_RIGHT: return ">>";
	case BINOP_CAST: return "(as)";
	default: return "(?)";
	}
}

static const char* ee_gen_unop_to_c(Ast_Unop_Type type)
{
	switch (type)
	{
	case UNOP_MINUS: return "-";
	case UNOP_NOT: return "!";
	case UNOP_BW_NOT: return "~";
	case UNOP_PTR: return "&";
	case UNOP_DEREF: return "*";
	default: return "(?)";
	}
}

Codegen ee_gen_new(const Sem_Scope* scope, const Ast_Module* mod, const char* file_out)
{
	Codegen gen = { 0 };

	gen.scope = (Sem_Scope*)scope;
	gen.mod = (Ast_Module*)mod;
	gen.file_out = file_out;

	gen.handle = fopen(file_out, "w");

	if (gen.handle == NULL)
	{
		fprintf(stderr, "Failed to open output file: %s\n", file_out);
	}

	return gen;
}

void ee_gen_prologue(Codegen* gen)
{
	fwrite(_s_include_prol, 1, _s_include_prol_len, gen->handle);
	fwrite(_s_typedef_prol, 1, _s_typedef_prol_len, gen->handle);
}

void ee_gen_run(Codegen* gen)
{
	if (gen->handle == NULL) 
		return;

	ee_gen_prologue(gen);
	ee_gen_stmt(gen, gen->mod->root, gen->scope, 0);

	fclose(gen->handle);
}
void ee_gen_type(Codegen* gen, Sem_Type* type)
{
	if (type == NULL)
	{
		ee_gen_printf(gen, "void");
		return;
	}

	switch (type->type)
	{
	case SEM_TYPE_ERROR:
	{
		ee_gen_printf(gen, "void /* ERROR */");
	} break;
	case SEM_TYPE_PRIMITIVE:
	{
		ee_gen_printf(gen, "%s", _s_dtype_names[type->as_primitive.dtype]);
	} break;
	case SEM_TYPE_PTR:
	{
		ee_gen_type(gen, type->as_ptr.to);
		ee_gen_printf(gen, "*");
	} break;
	case SEM_TYPE_ARRAY:
	{
		ee_gen_type(gen, type->as_array.elem_type);
	} break;
	case SEM_TYPE_STRUCT:
	{
		ee_gen_printf(gen, "struct { ");

		Sem_Struct_Member* members = (Sem_Struct_Member*)type->as_struct.members.buffer;

		for (size_t i = 0; i < ee_array_len(&type->as_struct.members); ++i)
		{
			ee_gen_type(gen, members[i].type);
			ee_gen_printf(gen, " %.*s; ", (int)members[i].ident->scratch.len, members[i].ident->scratch.buffer);
		}

		ee_gen_printf(gen, "}");
	} break;
	case SEM_TYPE_TUPLE:
	{
		ee_gen_printf(gen, "struct { ");

		Sem_Type** types = (Sem_Type**)type->as_tuple.types.buffer;

		for (size_t i = 0; i < ee_array_len(&type->as_tuple.types); ++i)
		{
			ee_gen_type(gen, types[i]);
			ee_gen_printf(gen, " _%zu; ", i);
		}

		ee_gen_printf(gen, "}");
	} break;
	case SEM_TYPE_UNION:
	{
		ee_gen_printf(gen, "union { ");
		Sem_Type** types = (Sem_Type**)type->as_union.types.buffer;

		for (size_t i = 0; i < ee_array_len(&type->as_union.types); ++i)
		{
			ee_gen_type(gen, types[i]);
			ee_gen_printf(gen, " _%zu; ", i);
		}

		ee_gen_printf(gen, "}");
	} break;
	case SEM_TYPE_FUNC:
	{
		ee_gen_type(gen, type->as_func.ret);
		ee_gen_printf(gen, " (*)");
		ee_gen_printf(gen, "(");

		Sem_Type** params = (Sem_Type**)type->as_func.params.buffer;

		for (size_t i = 0; i < ee_array_len(&type->as_func.params); ++i)
		{
			ee_gen_type(gen, params[i]);

			if (i < ee_array_len(&type->as_func.params) - 1)
				ee_gen_printf(gen, ", ");
		}

		ee_gen_printf(gen, ")");
	} break;
	}
}
void ee_gen_expr(Codegen* gen, Ast_Expr* expr)
{
	switch (expr->type)
	{
	case EXPR_LIT:
	{
		const Token* token = expr->as_lit.token;
		switch (token->type)
		{
		case TOKEN_LIT_INT: ee_gen_printf(gen, "%llu", token->as_u64); break;
		case TOKEN_LIT_FLOAT: ee_gen_printf(gen, "%f", token->as_f64); break;
		case TOKEN_LIT_STR: ee_gen_printf(gen, "\"%.*s\"", (int)token->as_str_view.len, token->as_str_view.buffer); break;
		case TOKEN_TRUE: ee_gen_printf(gen, "true"); break;
		case TOKEN_FALSE: ee_gen_printf(gen, "false"); break;
		default: break;
		}
	} break;
	case EXPR_IDENT:
	{
		ee_gen_print_token(gen, expr->as_ident.token);
	} break;
	case EXPR_BINOP:
	{
		if (expr->as_binop.type == BINOP_CAST)
		{
			ee_gen_printf(gen, "(");
			ee_gen_expr(gen, expr->as_binop.right);
			ee_gen_printf(gen, ")(");
			ee_gen_expr(gen, expr->as_binop.left);
			ee_gen_printf(gen, ")");
		}
		else
		{
			ee_gen_printf(gen, "(");
			ee_gen_expr(gen, expr->as_binop.left);
			ee_gen_printf(gen, " %s ", ee_gen_binop_to_c(expr->as_binop.type));
			ee_gen_expr(gen, expr->as_binop.right);
			ee_gen_printf(gen, ")");
		}
	} break;
	case EXPR_UNOP:
	{
		ee_gen_printf(gen, "(%s(", ee_gen_unop_to_c(expr->as_unop.type));
		ee_gen_expr(gen, expr->as_unop.expr);
		ee_gen_printf(gen, "))");
	} break;
	case EXPR_FUNC_CALL:
	{
		ee_gen_expr(gen, expr->as_func_call.func);
		ee_gen_printf(gen, "(");

		Ast_Expr** args = (Ast_Expr**)expr->as_func_call.args.buffer;
		
		for (size_t i = 0; i < ee_array_len(&expr->as_func_call.args); ++i)
		{
			ee_gen_expr(gen, args[i]);
		
			if (i < ee_array_len(&expr->as_func_call.args) - 1)
				ee_gen_printf(gen, ", ");
		}
		
		ee_gen_printf(gen, ")");
	} break;

	case EXPR_ACCESS:
	{
		ee_gen_expr(gen, expr->as_access.entity);
		ee_gen_printf(gen, ".");
		ee_gen_print_token(gen, expr->as_access.member);
	} break;
	case EXPR_INDEX:
	{
		ee_gen_expr(gen, expr->as_index.entity);
		ee_gen_printf(gen, "[");
		ee_gen_expr(gen, expr->as_index.index);
		ee_gen_printf(gen, "]");
	} break;

	case EXPR_TYPE_STRUCT:
	case EXPR_TYPE_TUPLE:
	case EXPR_TYPE_UNION:
	case EXPR_TYPE_ARRAY:
		break;

	default:
		break;
	}
}

void ee_gen_stmt(Codegen* gen, Ast_Stmt* stmt, Sem_Scope* scope, size_t indent)
{
	if (stmt == NULL)
	{
		ee_gen_print_indent(gen, indent);
		ee_gen_printf(gen, "/* (NULL STMT) */;\n");
		EE_ASSERT(0, "Codegen: ee_gen_stmt received NULL statement");
		return;
	}

	ee_gen_print_indent(gen, indent);

	switch (stmt->type)
	{
	case STMT_VAR_DECL:
	{
		Sem_Entry* entry = ee_scope_lookup_entry(scope, stmt->as_var_decl.ident);
		EE_ASSERT(entry != NULL, "Codegen: Undeclared variable? Semantic analysis failed.");
		Sem_Type* type = entry->type_info;

		if (ee_var_decl_has_flag(stmt, AST_CONST))
		{
			ee_gen_printf(gen, "const ");
		}

		ee_gen_type(gen, type);
		ee_gen_printf(gen, " ");
		ee_gen_print_token(gen, stmt->as_var_decl.ident);

		if (type->type == SEM_TYPE_ARRAY)
		{
			ee_gen_printf(gen, "[%llu]", type->as_array.count);
		}

		if (stmt->as_var_decl.val)
		{
			ee_gen_printf(gen, " = ");
			ee_gen_expr(gen, stmt->as_var_decl.val);
		}
		ee_gen_printf(gen, ";\n");
	} break;

	case STMT_FN:
	{
		Sem_Entry* entry = ee_scope_lookup_entry(scope, stmt->as_func_decl.ident);
		EE_ASSERT(entry != NULL && entry->type_info->type == SEM_TYPE_FUNC, "Codegen: Function semantic info not found.");
		Sem_Type* func_type = entry->type_info;

		ee_gen_type(gen, func_type->as_func.ret);
		ee_gen_printf(gen, " ");
		ee_gen_print_token(gen, stmt->as_func_decl.ident);
		ee_gen_printf(gen, "(");

		const Token** param_idents = (const Token**)stmt->as_func_decl.param_idents.buffer;
		Sem_Type** param_types = (Sem_Type**)func_type->as_func.params.buffer;
		size_t param_count = ee_array_len(&func_type->as_func.params);

		for (size_t i = 0; i < param_count; ++i)
		{
			ee_gen_type(gen, param_types[i]);
			ee_gen_printf(gen, " ");
			ee_gen_print_token(gen, param_idents[i]);

			if (i < param_count - 1)
				ee_gen_printf(gen, ", ");
		}
		ee_gen_printf(gen, ")\n");

		EE_ASSERT(scope->top < ee_array_len(&scope->children), "Codegen: Scope mismatch for function body");
		Sem_Scope* func_scope = ((Sem_Scope**)scope->children.buffer)[scope->top++];

		ee_gen_stmt(gen, stmt->as_func_decl.body, func_scope, indent);
		ee_gen_printf(gen, "\n");
	} break;
	case STMT_BLOCK:
	{
		EE_ASSERT(scope->top < ee_array_len(&scope->children), "Codegen: Scope mismatch for block");

		Sem_Scope* block_scope = ((Sem_Scope**)scope->children.buffer)[scope->top++];
		Bool not_global = scope->parent != NULL;

		if (not_global)
		{
			ee_gen_printf(gen, "{\n");
		}

		Ast_Stmt** stmts = (Ast_Stmt**)stmt->as_block.stmts.buffer;
		for (size_t i = 0; i < ee_array_len(&stmt->as_block.stmts); ++i)
		{
			ee_gen_stmt(gen, stmts[i], block_scope, indent + not_global);
		}

		if (not_global)
		{
			ee_gen_print_indent(gen, indent);
			ee_gen_printf(gen, "}\n");
		}
	} break;
	case STMT_IF:
	{
		ee_gen_printf(gen, "if (");
		ee_gen_expr(gen, stmt->as_if.cond);
		ee_gen_printf(gen, ")\n");

		ee_gen_stmt(gen, stmt->as_if.if_block, scope, indent);

		if (stmt->as_if.else_block)
		{
			ee_gen_print_indent(gen, indent);
			ee_gen_printf(gen, "else\n");
			ee_gen_stmt(gen, stmt->as_if.else_block, scope, indent);
		}
	} break;
	case STMT_FOR:
	{
		EE_ASSERT(stmt->as_for.range->type == EXPR_BINOP && stmt->as_for.range->as_binop.type == BINOP_RANGE, "Codegen: Invalid FOR range");

		EE_ASSERT(scope->top < ee_array_len(&scope->children), "Codegen: Scope mismatch for 'for' block");
		Sem_Scope* for_scope = ((Sem_Scope**)scope->children.buffer)[scope->top++];
		Sem_Entry* it_entry = ee_scope_lookup_entry(for_scope, stmt->as_for.it);

		ee_gen_printf(gen, "for (");
		ee_gen_type(gen, it_entry->type_info);
		ee_gen_printf(gen, " ");
		ee_gen_print_token(gen, it_entry->ident);
		ee_gen_printf(gen, " = ");
		ee_gen_expr(gen, stmt->as_for.range->as_binop.left);
		ee_gen_printf(gen, "; ");
		ee_gen_print_token(gen, it_entry->ident);
		ee_gen_printf(gen, " < ");
		ee_gen_expr(gen, stmt->as_for.range->as_binop.right);
		ee_gen_printf(gen, "; ");
		ee_gen_print_token(gen, it_entry->ident);
		ee_gen_printf(gen, "++)\n");

		ee_gen_stmt(gen, stmt->as_for.body, for_scope, indent);
	} break;
	case STMT_WHILE:
	{
		// TODO(eesuck): in future maybe while block will create a scope

		ee_gen_printf(gen, "while (");
		ee_gen_expr(gen, stmt->as_while.cond);
		ee_gen_printf(gen, ")\n");
		ee_gen_stmt(gen, stmt->as_while.body, scope, indent);
	} break;
	case STMT_EXPR:
	{
		ee_gen_expr(gen, stmt->as_expr.expr);
		ee_gen_printf(gen, ";\n");
	} break;
	case STMT_RETURN:
	{
		ee_gen_printf(gen, "return");
		if (stmt->as_ret.val)
		{
			ee_gen_printf(gen, " ");
			ee_gen_expr(gen, stmt->as_ret.val);
		}
		ee_gen_printf(gen, ";\n");
	} break;
	case STMT_ASSIGN:
	{
		ee_gen_expr(gen, stmt->as_assign.ident);
		ee_gen_printf(gen, " = ");
		ee_gen_expr(gen, stmt->as_assign.val);
		ee_gen_printf(gen, ";\n");
	} break;
	default:
	{
		const Token* bad_token = ee_stmt_get_token(stmt);
		if (bad_token)
		{
			fprintf(stderr, "Codegen: PANIC! Unknown statement type %d at token '%.*s'\n",
				stmt->type, (int)bad_token->scratch.len, bad_token->scratch.buffer);
		}
		else
		{
			fprintf(stderr, "Codegen: PANIC! Unknown statement type %d (no token info)\n", stmt->type);
		}
		EE_ASSERT(0, "Codegen: Unknown statement type");
	} break;
	}
}