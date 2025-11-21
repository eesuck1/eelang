#include "ee_arena.h"

#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_log.h"

int main()
{
	const char* file_path = "assets/test.ee";

	Linked_Arena linked_arena = ee_linked_arena_new(EE_NMB(1), EE_NO_REWIND, NULL);
	Allocator allocator = ee_linked_arena_allocator(&linked_arena);

	Lexer lex = ee_lex_new_file(file_path, &allocator);
	ee_lex_tokenize(&lex);

	Logger log = { &lex };
	Parser pars = ee_pars_new(&lex, log, &lex.allocator);

	Ast_Module* mod = ee_pars_run(&pars);
	ee_pars_debug_print_module(mod);

	ee_linked_arena_free(&linked_arena);

	return 0;
}