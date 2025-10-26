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

#endif // EE_C_CODEGEN_H