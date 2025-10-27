#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_parser.c"
#include "ee_c_codegen.h"
#include "ee_c_codegen.c"

int main()
{
	const char* file_path = "assets/eelang_test_let.eel";

	Lexer lex = ee_lex_new_file(file_path, NULL);

	ee_lex_tokenize(&lex);

	Parser pars = ee_pars_new(&lex.tokens, &lex.allocator);
	
	//Ast_Type_Info* t = ee_pars_type_info(&pars);

	//ee_pars_debug_print_type_info(t, 0);
	//Ast_Expr* res_0 = ee_pars_expr(&pars);
	//
	//ee_pars_debug_print_expr(res_0, 0);

	Ast_Stmt* st = ee_pars_stmt(&pars);

	ee_pars_debug_print_stmt(st, 0);

	return 0;
}