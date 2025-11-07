#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_parser.c"
#include "ee_log.h"
#include "ee_log.c"
#include "ee_semantic.h"
#include "ee_semantic.c"

int main()
{
	const char* file_path = "assets/eelang_test_expr.ee";

	Lexer lex = ee_lex_new_file(file_path, NULL);
	ee_lex_tokenize(&lex);

	Logger log = { 0 };
	log.lexer = &lex;

	//ee_log_error_token(&log, (const Token*)ee_array_at(&lex.tokens, 1), "Test error message: %d", 123);

	Parser pars = ee_pars_new(&lex, log, &lex.allocator);

	Ast_Module* mod = ee_pars_run(&pars);
	ee_pars_debug_print_module(mod);

	Sem_Analyzer sem = ee_sem_new(mod, log, &lex.allocator);
	ee_sem_resolve_scopes(&sem);

	return 0;
}