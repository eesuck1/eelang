#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_parser.c"
#include "ee_log.h"
#include "ee_log.c"

int main()
{
	const char* file_path = "assets/eelang_test_log.eel";

	Lexer lex = ee_lex_new_file(file_path, NULL);
	ee_lex_tokenize(&lex);

	Logger log = { 0 };
	log.lexer = &lex;

	ee_log_error_token(&log, (const Token*)ee_array_at(&lex.tokens, 1), "Test error message: %d", 123);

	//Parser pars = ee_pars_new(&lex.tokens, &lex.allocator);
	
	//Ast_Type_Info* t = ee_pars_type_info(&pars);
	//ee_pars_debug_print_type_info(t, 0);

	//Ast_Stmt* st = ee_pars_stmt(&pars);
	//ee_pars_debug_print_stmt(st, 0);

	return 0;
}