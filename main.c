#include "ee_arena.h"

#include "ee_lexer.h"
#include "ee_parser.h"
#include "ee_log.h"
#include "ee_ir.h"

int main()
{
	const char* file_path = "assets/test_import.ee";

	Linked_Arena linked_arena = ee_linked_arena_new(EE_NMB(1), EE_NO_REWIND, NULL);
	Allocator allocator = ee_linked_arena_allocator(&linked_arena);
	
	//Parse_Project paq = ee_pars_queue_run(file_path, &allocator);

	//ee_pars_debug_print_project(&paq);
	//ee_linked_arena_free(&linked_arena);

	Virtual_Machine vm = ee_vm_new(1024, 1024, &allocator);
	VM_Program prog = ee_vm_prog_new(32, 32, 1024, &allocator);

	VM_Index c_0 = ee_vm_prog_push_const(&prog, (VM_Val) { .as_u64 = 10 });
	VM_Index c_1 = ee_vm_prog_push_const(&prog, (VM_Val) { .as_u64 = 1 });
	VM_Index c_2 = ee_vm_prog_push_const(&prog, (VM_Val) { .as_u64 = 4 });

	ee_vm_prog_push_op_1(&prog, OP_ALLOCA, 2);
	ee_vm_prog_push_op_2(&prog, OP_MOVI, 0, c_0);
	ee_vm_prog_push_op_2(&prog, OP_MOVI, 1, c_1);
	ee_vm_prog_push_op_3(&prog, OP_SLT, 3, 2, 0);
	ee_vm_prog_push_op_3(&prog, OP_ADD, 2, 2, 1);
	ee_vm_prog_push_op_2(&prog, OP_JNZ, 3, 3);
	ee_vm_prog_push_op_0(&prog, OP_HALT);

	ee_vm_prog_debug_print(&prog);
	ee_vm_run(&vm, &prog);
	ee_vm_debug_print(&vm);

	return 0;
}