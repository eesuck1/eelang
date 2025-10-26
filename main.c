#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_parser.c"
#include "ee_c_codegen.h"
#include "ee_c_codegen.c"

int main()
{
	const char* file_path = "C:/Users/volod/Documents/git/eelang/assets/eelang_test_expr.eel";

	Lexer lex = ee_lex_new_file(file_path, NULL);

	ee_lex_tokenize(&lex);

	Parser pars = ee_pars_new(&lex.tokens, &lex.allocator);
	Ast_Expr* res_0 = ee_pars_expr(&pars);
	
	ee_pars_debug_print_expr(res_0, 0);

	return 0;
}