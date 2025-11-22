#include "ee_arena.h"

#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_log.h"

int main()
{
	const char* file_path = "assets/test_import.ee";

	Linked_Arena linked_arena = ee_linked_arena_new(EE_NMB(1), EE_NO_REWIND, NULL);
	Allocator allocator = ee_linked_arena_allocator(&linked_arena);
	Parse_Project paq = ee_pars_queue_run(file_path, &allocator);

	ee_pars_debug_print_project(&paq);
	ee_linked_arena_free(&linked_arena);

	return 0;
}