#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_parser.c"
#include "ee_c_codegen.h"
#include "ee_c_codegen.c"

int main()
{
	const char* file_path = "C:\\Users\\IsakVolodymy\\Documents\\eelang\\assets\\eelang_test_0.eel";

	Lexer lex = ee_lex_new_file(file_path, NULL);

	ee_lex_tokenize(&lex);

	for (i32 i = 0; i < ee_array_len(&lex.tokens); ++i)
	{
		Token* token = (Token*)ee_array_at(&lex.tokens, i);

		EE_PRINT("<");
		for (i32 j = 0; j < token->scratch.len; ++j)
		{
			EE_PRINT("%c", token->scratch.buffer[j]);
		}
		EE_PRINTLN(": %d>", token->type);
	}

	Parser pars = ee_pars_new(&lex.tokens, &lex.allocator);

	ee_pars_run(&pars);

	{
		Ast_Node* node = (Ast_Node*)ee_array_at(&pars.nodes, 0);

		EE_PRINT("<");
		for (i32 j = 0; j < node->as_id.name.len; ++j)
		{
			EE_PRINT("%c", node->as_id.name.buffer[j]);
		}
		EE_PRINT(" = ");
		if (node->as_id.value != EE_AST_NODE_NULL)
		{
			Ast_Node* val = (Ast_Node*)ee_array_at(&pars.nodes, node->as_id.value);

			EE_PRINT("%llu", val->as_lit.as_u64);
		}
		EE_PRINTLN(": %d>", node->type);
	}

	{
		Ast_Node* node = (Ast_Node*)ee_array_at(&pars.nodes, 2);

		EE_PRINT("<");
		for (i32 j = 0; j < node->as_id.name.len; ++j)
		{
			EE_PRINT("%c", node->as_id.name.buffer[j]);
		}
		EE_PRINT(" = ");
		if (node->as_id.value != EE_AST_NODE_NULL)
		{
			Ast_Node* val = (Ast_Node*)ee_array_at(&pars.nodes, node->as_id.value);

			EE_PRINT("%llu", val->as_lit.as_u64);
		}
		EE_PRINTLN(": %d>", node->type);
	}

	{
		Ast_Node* node = (Ast_Node*)ee_array_at(&pars.nodes, 4);

		EE_PRINT("<");
		for (i32 j = 0; j < node->as_id.name.len; ++j)
		{
			EE_PRINT("%c", node->as_id.name.buffer[j]);
		}
		EE_PRINT(" = ");
		if (node->as_id.value != EE_AST_NODE_NULL)
		{
			Ast_Node* val = (Ast_Node*)ee_array_at(&pars.nodes, node->as_id.value);

			EE_PRINT("%llu", val->as_lit.as_u64);
		}
		EE_PRINTLN(": %d>", node->type);
	}

	return 0;
}