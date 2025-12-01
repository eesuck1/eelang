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
							    
#define EE_VM_INVALID_REG    (0x0FFFFFFF)
#define EE_VM_IMM_FLAG       (0x80000000)
#define EE_VM_IMM_MASK       (~EE_VM_IMM_FLAG)
#define EE_VM_IMM_SIGN       (0x40000000)
#define EE_VM_IMM_MAX        (INT_MAX / 2)
#define EE_VM_IMM_MIN        (INT_MIN / 2)

#define EE_VM_CALL_FRAME_BASE_SIZE    (128)

typedef u32 VM_Word;

typedef enum VM_Op_Code
{
	OP_HALT = 0,

	OP_ALLOCA,
	OP_MOV,
	OP_MOVI,

	OP_ADD,
	OP_ADDI,
	OP_SUB,
	OP_SUBI,
	OP_MUL,
	OP_MULI,
	OP_IDIV,
	OP_IDIVI,
	OP_UDIV,
	OP_UDIVI,
	OP_IMOD,
	OP_IMODI,
	OP_UMOD,
	OP_UMODI,

	OP_FADD,
	OP_FADDI,
	OP_FSUB,
	OP_FSUBI,
	OP_FMUL,
	OP_FMULI,
	OP_FDIV,
	OP_FDIVI,

	OP_DADD,
	OP_DADDI,
	OP_DSUB,
	OP_DSUBI,
	OP_DMUL,
	OP_DMULI,
	OP_DDIV,
	OP_DDIVI,

	OP_NOT,
	OP_OR,
	OP_ORI,
	OP_AND,
	OP_ANDI,
	OP_XOR,
	OP_XORI,
	OP_SHL,
	OP_SHLI,
	OP_SHR,
	OP_SHRI,
	OP_SAR,
	OP_SARI,
	OP_SAL,
	OP_SALI,

	OP_JMP,
	OP_JMPI,
	OP_JIZ,
	OP_JIZI,
	OP_JNZ,
	OP_JNZI,

	OP_SEQ,
	OP_SEQI,
	OP_SNEQ,
	OP_SNEQI,
	OP_SLEQ,
	OP_SLEQI,
	OP_SGEQ,
	OP_SGEQI,
	OP_SLT,
	OP_SLTI,
	OP_SGT,
	OP_SGTI,

	OP_SLEQU,
	OP_SLEQUI,
	OP_SGEQU,
	OP_SGEQUI,
	OP_SLTU,
	OP_SLTUI,
	OP_SGTU,
	OP_SGTUI,

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
	VM_Word r_d;
	VM_Word r_1;
	VM_Word r_0;
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

typedef struct VM_Program
{
	Linked_Array ops;     // VM_Op
	Linked_Array consts;  // VM_Val
	Linked_Arena data;    // u8
	Dict symbols;         // VM_Word -> Token* ?
	Allocator allocator;
} VM_Program;

typedef struct VM_Call_Frame
{
	VM_Word ip;
	VM_Word bp;
} VM_Call_Frame;

typedef struct Virtual_Machine
{
	VM_Word ip;
	VM_Word sp;
	VM_Word bp;
	Bool halt;
	size_t ticks;

	Linked_Array stack;    // VM_Val
	Linked_Array frames;   // VM_Call_Frame
	Linked_Arena heap;
	Allocator allocator;
} Virtual_Machine;

VM_Program ee_vm_prog_new(size_t ops_count, size_t consts_count, size_t data_bytes, const Allocator* allocator);
VM_Op ee_vm_prog_get_op(VM_Program* prog, size_t i);
VM_Word ee_vm_prog_make_const(VM_Program* prog, i64 val);
void ee_vm_prog_push_op(VM_Program* prog, VM_Op_Code code, VM_Word r_d, VM_Word r_1, VM_Word r_0);
VM_Val ee_vm_prog_const_at(VM_Program* prog, VM_Word i);
VM_Val ee_vm_prog_get_imm(VM_Program* prog, VM_Word val);
VM_Word ee_vm_prog_push_const(VM_Program* prog, VM_Val const_val);

Virtual_Machine ee_vm_new(size_t stack_size, size_t heap_size, const Allocator* allocator);
VM_Val ee_vm_stack_at(Virtual_Machine* vm, VM_Word i);
void ee_vm_stack_set(Virtual_Machine* vm, VM_Word i, VM_Val val);
void ee_vm_run(Virtual_Machine* vm, const VM_Program* prog);
void ee_vm_debug_print(const Virtual_Machine* vm);
void ee_vm_prog_debug_print(const VM_Program* prog);

#endif // EE_IR_H
