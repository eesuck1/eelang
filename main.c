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
	Parse_Project paq = ee_pars_queue_run(file_path, &allocator);

	ee_pars_debug_print_project(&paq);
	ee_linked_arena_free(&linked_arena);

	//Virtual_Machine vm = ee_vm_new(1024, 1024, &allocator);
	//VM_Program prog = ee_vm_prog_new(32, 32, 1024, &allocator);

	//ee_vm_prog_push_op(&prog, OP_ALLOCAI, ee_vm_prog_make_const(&prog, 3), EE_VM_INVALID_REG, EE_VM_INVALID_REG);
	//ee_vm_prog_push_op(&prog, OP_MOVI, 0, ee_vm_prog_make_const(&prog, 0x000000FF), EE_VM_INVALID_REG);
	//ee_vm_prog_push_op(&prog, OP_MOVI, 1, ee_vm_prog_make_const(&prog, 0x0000FF00), EE_VM_INVALID_REG);
	//ee_vm_prog_push_op(&prog, OP_REGCPY8I, 0, ee_vm_prog_make_const(&prog, 1), 1);
	//ee_vm_prog_push_op(&prog, OP_MOV, 2, 1, EE_VM_INVALID_REG);
	//ee_vm_prog_push_op(&prog, OP_REGSET32I, 2, ee_vm_prog_make_const(&prog, 1), ee_vm_prog_make_const(&prog, 0x1212));
	//ee_vm_prog_push_op(&prog, OP_HALT, EE_VM_INVALID_REG, EE_VM_INVALID_REG, EE_VM_INVALID_REG);

	//ee_vm_prog_debug_print(&prog);
	//ee_vm_run(&vm, &prog);
	//ee_vm_debug_print(&vm);

	return 0;
 }