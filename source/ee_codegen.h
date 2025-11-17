#ifndef EE_CODEGEN_H
#define EE_CODEGEN_H

#include "ee_parser.h"
#include "ee_semantic.h"

typedef struct Codegen
{
	Sem_Scope* scope;
	Ast_Module* mod;
	FILE* handle;

	const char* file_out;
} Codegen;

Codegen ee_gen_new(const Sem_Scope* scope, const Ast_Module* mod, const char* file_out);
const Token* ee_stmt_get_token(const Ast_Stmt* stmt);

void ee_gen_run(Codegen* gen);
void ee_gen_prologue(Codegen* gen);
void ee_gen_type(Codegen* gen, Sem_Type* type);
void ee_gen_expr(Codegen* gen, Ast_Expr* expr);
void ee_gen_stmt(Codegen* gen, Ast_Stmt* stmt, Sem_Scope* scope, size_t indent);

#endif