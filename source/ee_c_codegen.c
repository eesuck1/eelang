#include "ee_c_codegen.h"

C_Codegen ee_cgen_new(const Array* nodes, const char* file_out)
{
	C_Codegen out = { 0 };

	out.nodes = nodes;
	out.file_out = file_out;

	return out;
}

void ee_cgen_run(C_Codegen* cgen)
{
	FILE* file = fopen(cgen->file_out, "wb");

	EE_ASSERT(file != NULL, "Unable to create output file (%s)", cgen->file_out);

	const Ast_Node* current = ee_cgen_node_at(cgen, 0);

	while (current->type != NODE_EOF)
	{

	}
}
