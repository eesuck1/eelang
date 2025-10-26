#include "ee_c_codegen.h"

C_Codegen ee_cgen_new(const Array* nodes, const char* file_out)
{
	C_Codegen out = { 0 };

	out.nodes = nodes;
	out.file_out = file_out;

	return out;
}