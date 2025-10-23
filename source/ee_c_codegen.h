#ifndef EE_C_CODEGEN_H
#define EE_C_CODEGEN_H

#include "ee_parser.h"

typedef struct C_Codegen
{
	const Array* nodes;
	const char*  file_out;
} C_Codegen;

C_Codegen ee_cgen_new(const Array* nodes, const char* file_out);
void ee_cgen_run(C_Codegen* cgen);

EE_INLINE const Ast_Node* ee_cgen_node_at(C_Codegen* cgen, Ast_Node_Handle handle)
{
	return (const Ast_Node*)ee_array_at(cgen->nodes, handle);
}

#endif // EE_C_CODEGEN_H