#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_log.h"

int main()
{
	const char* file_path = "assets/test.ee";

	Lexer lex = ee_lex_new_file(file_path, NULL);
	ee_lex_tokenize(&lex);

	Logger log = { 0 };
	log.lexer = &lex;

	Parser pars = ee_pars_new(&lex, log, &lex.allocator);

	Ast_Module* mod = ee_pars_run(&pars);
	ee_pars_debug_print_module(mod);

	return 0;
}