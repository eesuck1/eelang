#ifndef EE_CODEGEN_H
#define EE_CODEGEN_H

#include "ee_parser.h"
#include "ee_semantic.h"

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

typedef struct Codegen
{
	Sem_Scope* scope;
	Ast_Module* mod;
	FILE* handle;
	
	const char* file_out;
} Codegen;

Codegen ee_gen_new(const Sem_Scope* scope, const Ast_Module* mod, const char* file_out);
void ee_gen_run(Codegen* gen);
void ee_gen_prologue(Codegen* gen);
void ee_gen_type(Codegen* gen, Sem_Type* type);
void ee_gen_expr(Codegen* gen, Ast_Expr* expr);
void ee_gen_stmt(Codegen* gen, Ast_Stmt* stmt, Sem_Scope* scope);


#endif // EE_CODEGEN_H