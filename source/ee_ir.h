#ifndef EE_IR_H
#define EE_IR_H

#include "ee_parser.h"

#define EE_VM_MASK_32_0      (0x0000FFFF)
#define EE_VM_MASK_32_1      (0xFFFF0000)
						     
#define EE_VM_MASK_16_0      (0x000000FF)
#define EE_VM_MASK_16_1      (0x0000FF00)
#define EE_VM_MASK_16_2      (0x00FF0000)
#define EE_VM_MASK_16_3      (0xFF000000)
						     
#define EE_VM_MASK_8_0       (0x0000000F)
#define EE_VM_MASK_8_1       (0x000000F0)
#define EE_VM_MASK_8_2       (0x00000F00)
#define EE_VM_MASK_8_3       (0x0000F000)
#define EE_VM_MASK_8_4       (0x000F0000)
#define EE_VM_MASK_8_5       (0x00F00000)
#define EE_VM_MASK_8_6       (0x0F000000)
#define EE_VM_MASK_8_7       (0xF0000000)

#define EE_VM_INVALID_REG    (0xFFFFFFFF)

typedef u32 VM_Index;

typedef enum VM_Op_Code
{
	OP_HALT = 0,

	OP_ALLOCA,  
	OP_MOV,     
	OP_MOVI,    

	OP_ADD,     
	OP_SUB,     
	OP_MUL,     
	OP_IDIV,
	OP_UDIV,
	OP_FDIV,
	OP_DDIV,
		        
	OP_OR,      
	OP_AND,     
	OP_XOR,     
	OP_SHL,     
	OP_SHR,     

	OP_JMP,
	OP_JIZ,
	OP_JNZ,

	OP_SEQ,
	OP_SNEQ,
	OP_SLEQ,
	OP_SGEQ,
	OP_SLT,
	OP_SGT,

	OP_SEQU,
	OP_SNEQU,
	OP_SLEQU,
	OP_SGEQU,
	OP_SLTU,
	OP_SGTU,
	
	OP_ITOF,
	OP_FTOI,
	OP_ITOD,
	OP_DTOI,
	OP_SEXT8,
	OP_SEXT16,
	OP_SEXT32,

	OP_CALL,
	OP_RET,

	OP_COUNT
} VM_Op_Code;

typedef struct VM_Op
{
	VM_Op_Code code;
	VM_Index r_d;
	VM_Index r_1;
	VM_Index r_0;
} VM_Op;

typedef union VM_Val
{
	u8 as_u8; u16 as_u16; u32 as_u32; u64 as_u64;
	i8 as_i8; i16 as_i16; i32 as_i32; i64 as_i64;

	f32 as_f32; f64 as_f64;

	u8 as_bytes[8];
	u16 as_words[4];
	u32 as_dwords[2];

	void* as_anyptr;
} VM_Val;

typedef struct VM_Heap
{
	u8* buffer;
} VM_Heap;

typedef struct VM_Program
{
	Linked_Array ops;     // VM_Op
	Linked_Array consts;  // VM_Val
	Linked_Arena data;    // u8
	Dict symbols;         // VM_Index -> Token* ?
	Allocator allocator;
} VM_Program;

typedef struct Virtual_Machine
{
	VM_Index ip;
	VM_Index sp;
	VM_Index bp;
	Bool halt;
	size_t ticks;

	Linked_Array stack;   // VM_Val
	VM_Heap heap;
	Allocator allocator;
} Virtual_Machine;

VM_Program ee_vm_prog_new(size_t ops_count, size_t consts_count, size_t data_bytes, const Allocator* allocator);
VM_Op ee_vm_prog_get_op(VM_Program* prog, size_t i);
void ee_vm_prog_push_op_0(VM_Program* prog, VM_Op_Code code);
void ee_vm_prog_push_op_1(VM_Program* prog, VM_Op_Code code, VM_Index r_d);
void ee_vm_prog_push_op_2(VM_Program* prog, VM_Op_Code code, VM_Index r_d, VM_Index r_1);
void ee_vm_prog_push_op_3(VM_Program* prog, VM_Op_Code code, VM_Index r_d, VM_Index r_1, VM_Index r_0);
VM_Val ee_vm_prog_const_at(VM_Program* prog, VM_Index i);
VM_Index ee_vm_prog_push_const(VM_Program* prog, VM_Val const_val);

Virtual_Machine ee_vm_new(size_t stack_size, size_t heap_size, const Allocator* allocator);
VM_Val ee_vm_stack_at(Virtual_Machine* vm, VM_Index i);
void ee_vm_stack_set(Virtual_Machine* vm, VM_Index i, VM_Val val);
void ee_vm_run(Virtual_Machine* vm, const VM_Program* prog);
void ee_vm_debug_print(const Virtual_Machine* vm);
void ee_vm_prog_debug_print(const VM_Program* prog);

#endif // EE_IR_H
