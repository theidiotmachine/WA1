/** This file is a derived work of the parity-wasm project. 
 *
 * `parity-wasm` is primarily distributed under the terms of both the MIT
 * license and the Apache License (Version 2.0), at your choice.
 */

use crate::wasm::{WasmResultType};
use crate::wasm::wasm_serialize::{serialize_u32, serialize_i32, serialize_i64, serialize_f32, serialize_f64, serialize_u32_pad};
use crate::wasm::wasm_object_file::{WasmRelocationEntry, WasmObjectModuleFragment};

/// Instruction.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum WasmInstr {
	Unreachable,
	Nop,
	Block(WasmResultType),
	Loop(WasmResultType),
	If(WasmResultType),
	Else,
	End,
	Br(u32),
	BrIf(u32),
	BrTable(Vec<u32>, u32),
	Return,

	Call(u32),
	CallIndirect(u32, u8),

	Drop,
	Select,

	GetLocal(u32),
	SetLocal(u32),
	TeeLocal(u32),
	GetGlobal(u32),
	SetGlobal(u32),

	// All store/load instructions operate with 'memory immediates'
	// which represented here as (flag, offset) tuple
	I32Load(u32, u32),
	I64Load(u32, u32),
	F32Load(u32, u32),
	F64Load(u32, u32),
	I32Load8S(u32, u32),
	I32Load8U(u32, u32),
	I32Load16S(u32, u32),
	I32Load16U(u32, u32),
	I64Load8S(u32, u32),
	I64Load8U(u32, u32),
	I64Load16S(u32, u32),
	I64Load16U(u32, u32),
	I64Load32S(u32, u32),
	I64Load32U(u32, u32),
	I32Store(u32, u32),
	I64Store(u32, u32),
	F32Store(u32, u32),
	F64Store(u32, u32),
	I32Store8(u32, u32),
	I32Store16(u32, u32),
	I64Store8(u32, u32),
	I64Store16(u32, u32),
	I64Store32(u32, u32),

	CurrentMemory(u8),
	GrowMemory(u8),

	I32Const(i32),
	U32ConstStaticMemAddr(u32),
	I64Const(i64),
	F32Const(f32),
	F64Const(f64),

	I32Eqz,
	I32Eq,
	I32Ne,
	I32LtS,
	I32LtU,
	I32GtS,
	I32GtU,
	I32LeS,
	I32LeU,
	I32GeS,
	I32GeU,

	I64Eqz,
	I64Eq,
	I64Ne,
	I64LtS,
	I64LtU,
	I64GtS,
	I64GtU,
	I64LeS,
	I64LeU,
	I64GeS,
	I64GeU,

	F32Eq,
	F32Ne,
	F32Lt,
	F32Gt,
	F32Le,
	F32Ge,

	F64Eq,
	F64Ne,
	F64Lt,
	F64Gt,
	F64Le,
	F64Ge,

	I32Clz,
	I32Ctz,
	I32Popcnt,
	I32Add,
	I32Sub,
	I32Mul,
	I32DivS,
	I32DivU,
	I32RemS,
	I32RemU,
	I32And,
	I32Or,
	I32Xor,
	I32Shl,
	I32ShrS,
	I32ShrU,
	I32Rotl,
	I32Rotr,

	I64Clz,
	I64Ctz,
	I64Popcnt,
	I64Add,
	I64Sub,
	I64Mul,
	I64DivS,
	I64DivU,
	I64RemS,
	I64RemU,
	I64And,
	I64Or,
	I64Xor,
	I64Shl,
	I64ShrS,
	I64ShrU,
	I64Rotl,
	I64Rotr,
	F32Abs,
	F32Neg,
	F32Ceil,
	F32Floor,
	F32Trunc,
	F32Nearest,
	F32Sqrt,
	F32Add,
	F32Sub,
	F32Mul,
	F32Div,
	F32Min,
	F32Max,
	F32Copysign,
	F64Abs,
	F64Neg,
	F64Ceil,
	F64Floor,
	F64Trunc,
	F64Nearest,
	F64Sqrt,
	F64Add,
	F64Sub,
	F64Mul,
	F64Div,
	F64Min,
	F64Max,
	F64Copysign,

	I32WrapI64,
	I32TruncSF32,
	I32TruncUF32,
	I32TruncSF64,
	I32TruncUF64,
	I64ExtendSI32,
	I64ExtendUI32,
	I64TruncSF32,
	I64TruncUF32,
	I64TruncSF64,
	I64TruncUF64,
	F32ConvertSI32,
	F32ConvertUI32,
	F32ConvertSI64,
	F32ConvertUI64,
	F32DemoteF64,
	F64ConvertSI32,
	F64ConvertUI32,
	F64ConvertSI64,
	F64ConvertUI64,
	F64PromoteF32,

	I32ReinterpretF32,
	I64ReinterpretF64,
	F32ReinterpretI32,
	F64ReinterpretI64,

	#[cfg(feature="atomics")]
	Atomics(AtomicsInstruction),

	#[cfg(feature="simd")]
	Simd(SimdInstruction),

	#[cfg(feature="sign_ext")]
	SignExt(SignExtInstruction),

	#[cfg(feature="bulk")]
	Bulk(BulkInstruction),
}

#[allow(missing_docs)]
#[cfg(feature="atomics")]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AtomicsInstruction {
	AtomicWake(MemArg),
	I32AtomicWait(MemArg),
	I64AtomicWait(MemArg),

	I32AtomicLoad(MemArg),
	I64AtomicLoad(MemArg),
	I32AtomicLoad8u(MemArg),
	I32AtomicLoad16u(MemArg),
	I64AtomicLoad8u(MemArg),
	I64AtomicLoad16u(MemArg),
	I64AtomicLoad32u(MemArg),
	I32AtomicStore(MemArg),
	I64AtomicStore(MemArg),
	I32AtomicStore8u(MemArg),
	I32AtomicStore16u(MemArg),
	I64AtomicStore8u(MemArg),
	I64AtomicStore16u(MemArg),
	I64AtomicStore32u(MemArg),

	I32AtomicRmwAdd(MemArg),
	I64AtomicRmwAdd(MemArg),
	I32AtomicRmwAdd8u(MemArg),
	I32AtomicRmwAdd16u(MemArg),
	I64AtomicRmwAdd8u(MemArg),
	I64AtomicRmwAdd16u(MemArg),
	I64AtomicRmwAdd32u(MemArg),

	I32AtomicRmwSub(MemArg),
	I64AtomicRmwSub(MemArg),
	I32AtomicRmwSub8u(MemArg),
	I32AtomicRmwSub16u(MemArg),
	I64AtomicRmwSub8u(MemArg),
	I64AtomicRmwSub16u(MemArg),
	I64AtomicRmwSub32u(MemArg),

	I32AtomicRmwAnd(MemArg),
	I64AtomicRmwAnd(MemArg),
	I32AtomicRmwAnd8u(MemArg),
	I32AtomicRmwAnd16u(MemArg),
	I64AtomicRmwAnd8u(MemArg),
	I64AtomicRmwAnd16u(MemArg),
	I64AtomicRmwAnd32u(MemArg),

	I32AtomicRmwOr(MemArg),
	I64AtomicRmwOr(MemArg),
	I32AtomicRmwOr8u(MemArg),
	I32AtomicRmwOr16u(MemArg),
	I64AtomicRmwOr8u(MemArg),
	I64AtomicRmwOr16u(MemArg),
	I64AtomicRmwOr32u(MemArg),

	I32AtomicRmwXor(MemArg),
	I64AtomicRmwXor(MemArg),
	I32AtomicRmwXor8u(MemArg),
	I32AtomicRmwXor16u(MemArg),
	I64AtomicRmwXor8u(MemArg),
	I64AtomicRmwXor16u(MemArg),
	I64AtomicRmwXor32u(MemArg),

	I32AtomicRmwXchg(MemArg),
	I64AtomicRmwXchg(MemArg),
	I32AtomicRmwXchg8u(MemArg),
	I32AtomicRmwXchg16u(MemArg),
	I64AtomicRmwXchg8u(MemArg),
	I64AtomicRmwXchg16u(MemArg),
	I64AtomicRmwXchg32u(MemArg),

	I32AtomicRmwCmpxchg(MemArg),
	I64AtomicRmwCmpxchg(MemArg),
	I32AtomicRmwCmpxchg8u(MemArg),
	I32AtomicRmwCmpxchg16u(MemArg),
	I64AtomicRmwCmpxchg8u(MemArg),
	I64AtomicRmwCmpxchg16u(MemArg),
	I64AtomicRmwCmpxchg32u(MemArg),
}

#[allow(missing_docs)]
#[cfg(feature="simd")]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SimdInstruction {
	V128Const(Box<[u8; 16]>),
	V128Load(MemArg),
	V128Store(MemArg),
	I8x16Splat,
	I16x8Splat,
	I32x4Splat,
	I64x2Splat,
	F32x4Splat,
	F64x2Splat,
	I8x16ExtractLaneS(u8),
	I8x16ExtractLaneU(u8),
	I16x8ExtractLaneS(u8),
	I16x8ExtractLaneU(u8),
	I32x4ExtractLane(u8),
	I64x2ExtractLane(u8),
	F32x4ExtractLane(u8),
	F64x2ExtractLane(u8),
	I8x16ReplaceLane(u8),
	I16x8ReplaceLane(u8),
	I32x4ReplaceLane(u8),
	I64x2ReplaceLane(u8),
	F32x4ReplaceLane(u8),
	F64x2ReplaceLane(u8),
	V8x16Shuffle(Box<[u8; 16]>),
	I8x16Add,
	I16x8Add,
	I32x4Add,
	I64x2Add,
	I8x16Sub,
	I16x8Sub,
	I32x4Sub,
	I64x2Sub,
	I8x16Mul,
	I16x8Mul,
	I32x4Mul,
	// I64x2Mul,
	I8x16Neg,
	I16x8Neg,
	I32x4Neg,
	I64x2Neg,
	I8x16AddSaturateS,
	I8x16AddSaturateU,
	I16x8AddSaturateS,
	I16x8AddSaturateU,
	I8x16SubSaturateS,
	I8x16SubSaturateU,
	I16x8SubSaturateS,
	I16x8SubSaturateU,
	I8x16Shl,
	I16x8Shl,
	I32x4Shl,
	I64x2Shl,
	I8x16ShrS,
	I8x16ShrU,
	I16x8ShrS,
	I16x8ShrU,
	I32x4ShrS,
	I32x4ShrU,
	I64x2ShrS,
	I64x2ShrU,
	V128And,
	V128Or,
	V128Xor,
	V128Not,
	V128Bitselect,
	I8x16AnyTrue,
	I16x8AnyTrue,
	I32x4AnyTrue,
	I64x2AnyTrue,
	I8x16AllTrue,
	I16x8AllTrue,
	I32x4AllTrue,
	I64x2AllTrue,
	I8x16Eq,
	I16x8Eq,
	I32x4Eq,
	// I64x2Eq,
	F32x4Eq,
	F64x2Eq,
	I8x16Ne,
	I16x8Ne,
	I32x4Ne,
	// I64x2Ne,
	F32x4Ne,
	F64x2Ne,
	I8x16LtS,
	I8x16LtU,
	I16x8LtS,
	I16x8LtU,
	I32x4LtS,
	I32x4LtU,
	// I64x2LtS,
	// I64x2LtU,
	F32x4Lt,
	F64x2Lt,
	I8x16LeS,
	I8x16LeU,
	I16x8LeS,
	I16x8LeU,
	I32x4LeS,
	I32x4LeU,
	// I64x2LeS,
	// I64x2LeU,
	F32x4Le,
	F64x2Le,
	I8x16GtS,
	I8x16GtU,
	I16x8GtS,
	I16x8GtU,
	I32x4GtS,
	I32x4GtU,
	// I64x2GtS,
	// I64x2GtU,
	F32x4Gt,
	F64x2Gt,
	I8x16GeS,
	I8x16GeU,
	I16x8GeS,
	I16x8GeU,
	I32x4GeS,
	I32x4GeU,
	// I64x2GeS,
	// I64x2GeU,
	F32x4Ge,
	F64x2Ge,
	F32x4Neg,
	F64x2Neg,
	F32x4Abs,
	F64x2Abs,
	F32x4Min,
	F64x2Min,
	F32x4Max,
	F64x2Max,
	F32x4Add,
	F64x2Add,
	F32x4Sub,
	F64x2Sub,
	F32x4Div,
	F64x2Div,
	F32x4Mul,
	F64x2Mul,
	F32x4Sqrt,
	F64x2Sqrt,
	F32x4ConvertSI32x4,
	F32x4ConvertUI32x4,
	F64x2ConvertSI64x2,
	F64x2ConvertUI64x2,
	I32x4TruncSF32x4Sat,
	I32x4TruncUF32x4Sat,
	I64x2TruncSF64x2Sat,
	I64x2TruncUF64x2Sat,
}

#[allow(missing_docs)]
#[cfg(feature="sign_ext")]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SignExtInstruction {
	I32Extend8S,
	I32Extend16S,
	I64Extend8S,
	I64Extend16S,
	I64Extend32S,
}

#[allow(missing_docs)]
#[cfg(feature="bulk")]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BulkInstruction {
	MemoryInit(u32),
	MemoryDrop(u32),
	MemoryCopy,
	MemoryFill,
	TableInit(u32),
	TableDrop(u32),
	TableCopy,
}

macro_rules! op {
	($data: expr, $byte: expr) => ({
		let b: u8 = $byte;
		$data.push(b);
	});
	($data: expr, $byte: expr, $s: block) => ({
		op!($data, $byte);
		$s;
	});
}

/// This is one byte for the opcode, and five for the padded u32 sleb that is the number of functions in the 
/// expr, which is depressingly counted in the offset
const MAGIC_RELOC_OFFSET: u32 = 6;

impl WasmInstr {
	pub fn serialize_reloc(&self, reloc: &WasmObjectModuleFragment, reloc_entries: &mut Vec<WasmRelocationEntry>, writer: &mut Vec<u8>) {
		match self {
			WasmInstr::Call(index) => {
				reloc_entries.push(WasmRelocationEntry::new_call(writer.len() as u32 + MAGIC_RELOC_OFFSET, 
					reloc.linking_section.symbol_table.funcs[*index as usize]
				));
				op!(writer, opcodes::CALL, {
					serialize_u32_pad(*index, writer);
				});
			},
			WasmInstr::GetGlobal(index) => {
				reloc_entries.push(WasmRelocationEntry::new_global_use(writer.len() as u32 + MAGIC_RELOC_OFFSET, 
					reloc.linking_section.symbol_table.globals[*index as usize]
				));
				op!(writer, opcodes::GETGLOBAL, {
					serialize_u32_pad(*index, writer);
				});
			},
			WasmInstr::SetGlobal(index) => {
				reloc_entries.push(WasmRelocationEntry::new_global_use(writer.len() as u32 + MAGIC_RELOC_OFFSET, 
					reloc.linking_section.symbol_table.globals[*index as usize]
				));
				op!(writer, opcodes::SETGLOBAL, {
					serialize_u32_pad(*index, writer);
				})
			},
			WasmInstr::U32ConstStaticMemAddr(def) => {
				reloc_entries.push(WasmRelocationEntry::new_static_mem_const(writer.len() as u32 + MAGIC_RELOC_OFFSET, 
					reloc.linking_section.symbol_table.data[0], *def
				));
				op!(writer, opcodes::I32CONST, {
					serialize_u32_pad(*def, writer);
				})
			},
			_ => self.serialize(writer),
		}
	}

    pub fn serialize(&self, writer: &mut Vec<u8>) {
        use self::WasmInstr::*;
        use self::opcodes::*;
        match self {
			Unreachable => op!(writer, UNREACHABLE),
			Nop => op!(writer, NOP),
			Block(block_type) => op!(writer, BLOCK, {
				block_type.serialize(writer);
			}),
			Loop(block_type) => op!(writer, LOOP, {
				block_type.serialize(writer);
			}),
			If(block_type) => op!(writer, IF, {
				block_type.serialize(writer);
			}),
			Else => op!(writer, ELSE),
			End => op!(writer, END),
			Br(idx) => op!(writer, BR, {
				serialize_u32(*idx, writer);
			}),
			BrIf(idx) => op!(writer, BRIF, {
				serialize_u32(*idx, writer);
			}),
			BrTable(table, default) => op!(writer, BRTABLE, {
				serialize_u32(table.len() as u32, writer);
				for t in table {
					serialize_u32(*t, writer);	
				}
				serialize_u32(*default, writer);
			}),
			Return => op!(writer, RETURN),
			Call(index) => op!(writer, CALL, {
				serialize_u32(*index, writer);
			}),
			CallIndirect(index, reserved) => op!(writer, CALLINDIRECT, {
				serialize_u32(*index, writer);
				writer.push(*reserved);
			}),
			Drop => op!(writer, DROP),
			Select => op!(writer, SELECT),
			GetLocal(index) => op!(writer, GETLOCAL, {
				serialize_u32(*index, writer);
			}),
			SetLocal(index) => op!(writer, SETLOCAL, {
				serialize_u32(*index, writer);
			}),
			TeeLocal(index) => op!(writer, TEELOCAL, {
				serialize_u32(*index, writer);
			}),
			GetGlobal(index) => op!(writer, GETGLOBAL, {
				serialize_u32(*index, writer);
			}),
			SetGlobal(index) => op!(writer, SETGLOBAL, {
				serialize_u32(*index, writer);
			}),
			I32Load(flags, offset) => op!(writer, I32LOAD, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load(flags, offset) => op!(writer, I64LOAD, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			F32Load(flags, offset) => op!(writer, F32LOAD, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			F64Load(flags, offset) => op!(writer, F64LOAD, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Load8S(flags, offset) => op!(writer, I32LOAD8S, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Load8U(flags, offset) => op!(writer, I32LOAD8U, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Load16S(flags, offset) => op!(writer, I32LOAD16S, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Load16U(flags, offset) => op!(writer, I32LOAD16U, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load8S(flags, offset) => op!(writer, I64LOAD8S, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load8U(flags, offset) => op!(writer, I64LOAD8U, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load16S(flags, offset) => op!(writer, I64LOAD16S, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load16U(flags, offset) => op!(writer, I64LOAD16U, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load32S(flags, offset) => op!(writer, I64LOAD32S, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Load32U(flags, offset) => op!(writer, I64LOAD32U, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Store(flags, offset) => op!(writer, I32STORE, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Store(flags, offset) => op!(writer, I64STORE, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			F32Store(flags, offset) => op!(writer, F32STORE, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			F64Store(flags, offset) => op!(writer, F64STORE, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Store8(flags, offset) => op!(writer, I32STORE8, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I32Store16(flags, offset) => op!(writer, I32STORE16, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Store8(flags, offset) => op!(writer, I64STORE8, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Store16(flags, offset) => op!(writer, I64STORE16, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			I64Store32(flags, offset) => op!(writer, I64STORE32, {
				serialize_u32(*flags, writer);
				serialize_u32(*offset, writer);
			}),
			CurrentMemory(flag) => op!(writer, CURRENTMEMORY, {
				writer.push(*flag);
			}),
			GrowMemory(flag) => op!(writer, GROWMEMORY, {
				writer.push(*flag);
			}),
			I32Const(def) => op!(writer, I32CONST, {
				serialize_i32(*def, writer);
			}),
			U32ConstStaticMemAddr(def) => op!(writer, I32CONST, {
				serialize_u32(*def, writer);
			}),
			I64Const(def) => op!(writer, I64CONST, {
				serialize_i64(*def, writer);
			}),
			F32Const(def) => op!(writer, F32CONST, {
				serialize_f32(*def, writer);
			}),
			F64Const(def) => op!(writer, F64CONST, {
				serialize_f64(*def, writer);
			}),
			I32Eqz => op!(writer, I32EQZ),
			I32Eq => op!(writer, I32EQ),
			I32Ne => op!(writer, I32NE),
			I32LtS => op!(writer, I32LTS),
			I32LtU => op!(writer, I32LTU),
			I32GtS => op!(writer, I32GTS),
			I32GtU => op!(writer, I32GTU),
			I32LeS => op!(writer, I32LES),
			I32LeU => op!(writer, I32LEU),
			I32GeS => op!(writer, I32GES),
			I32GeU => op!(writer, I32GEU),

			I64Eqz => op!(writer, I64EQZ),
			I64Eq => op!(writer, I64EQ),
			I64Ne => op!(writer, I64NE),
			I64LtS => op!(writer, I64LTS),
			I64LtU => op!(writer, I64LTU),
			I64GtS => op!(writer, I64GTS),
			I64GtU => op!(writer, I64GTU),
			I64LeS => op!(writer, I64LES),
			I64LeU => op!(writer, I64LEU),
			I64GeS => op!(writer, I64GES),
			I64GeU => op!(writer, I64GEU),

			F32Eq => op!(writer, F32EQ),
			F32Ne => op!(writer, F32NE),
			F32Lt => op!(writer, F32LT),
			F32Gt => op!(writer, F32GT),
			F32Le => op!(writer, F32LE),
			F32Ge => op!(writer, F32GE),

			F64Eq => op!(writer, F64EQ),
			F64Ne => op!(writer, F64NE),
			F64Lt => op!(writer, F64LT),
			F64Gt => op!(writer, F64GT),
			F64Le => op!(writer, F64LE),
			F64Ge => op!(writer, F64GE),

			I32Clz => op!(writer, I32CLZ),
			I32Ctz => op!(writer, I32CTZ),
			I32Popcnt => op!(writer, I32POPCNT),
			I32Add => op!(writer, I32ADD),
			I32Sub => op!(writer, I32SUB),
			I32Mul => op!(writer, I32MUL),
			I32DivS => op!(writer, I32DIVS),
			I32DivU => op!(writer, I32DIVU),
			I32RemS => op!(writer, I32REMS),
			I32RemU => op!(writer, I32REMU),
			I32And => op!(writer, I32AND),
			I32Or => op!(writer, I32OR),
			I32Xor => op!(writer, I32XOR),
			I32Shl => op!(writer, I32SHL),
			I32ShrS => op!(writer, I32SHRS),
			I32ShrU => op!(writer, I32SHRU),
			I32Rotl => op!(writer, I32ROTL),
			I32Rotr => op!(writer, I32ROTR),

			I64Clz => op!(writer, I64CLZ),
			I64Ctz => op!(writer, I64CTZ),
			I64Popcnt => op!(writer, I64POPCNT),
			I64Add => op!(writer, I64ADD),
			I64Sub => op!(writer, I64SUB),
			I64Mul => op!(writer, I64MUL),
			I64DivS => op!(writer, I64DIVS),
			I64DivU => op!(writer, I64DIVU),
			I64RemS => op!(writer, I64REMS),
			I64RemU => op!(writer, I64REMU),
			I64And => op!(writer, I64AND),
			I64Or => op!(writer, I64OR),
			I64Xor => op!(writer, I64XOR),
			I64Shl => op!(writer, I64SHL),
			I64ShrS => op!(writer, I64SHRS),
			I64ShrU => op!(writer, I64SHRU),
			I64Rotl => op!(writer, I64ROTL),
			I64Rotr => op!(writer, I64ROTR),
			F32Abs => op!(writer, F32ABS),
			F32Neg => op!(writer, F32NEG),
			F32Ceil => op!(writer, F32CEIL),
			F32Floor => op!(writer, F32FLOOR),
			F32Trunc => op!(writer, F32TRUNC),
			F32Nearest => op!(writer, F32NEAREST),
			F32Sqrt => op!(writer, F32SQRT),
			F32Add => op!(writer, F32ADD),
			F32Sub => op!(writer, F32SUB),
			F32Mul => op!(writer, F32MUL),
			F32Div => op!(writer, F32DIV),
			F32Min => op!(writer, F32MIN),
			F32Max => op!(writer, F32MAX),
			F32Copysign => op!(writer, F32COPYSIGN),
			F64Abs => op!(writer, F64ABS),
			F64Neg => op!(writer, F64NEG),
			F64Ceil => op!(writer, F64CEIL),
			F64Floor => op!(writer, F64FLOOR),
			F64Trunc => op!(writer, F64TRUNC),
			F64Nearest => op!(writer, F64NEAREST),
			F64Sqrt => op!(writer, F64SQRT),
			F64Add => op!(writer, F64ADD),
			F64Sub => op!(writer, F64SUB),
			F64Mul => op!(writer, F64MUL),
			F64Div => op!(writer, F64DIV),
			F64Min => op!(writer, F64MIN),
			F64Max => op!(writer, F64MAX),
			F64Copysign => op!(writer, F64COPYSIGN),

			I32WrapI64 => op!(writer, I32WRAPI64),
			I32TruncSF32 => op!(writer, I32TRUNCSF32),
			I32TruncUF32 => op!(writer, I32TRUNCUF32),
			I32TruncSF64 => op!(writer, I32TRUNCSF64),
			I32TruncUF64 => op!(writer, I32TRUNCUF64),
			I64ExtendSI32 => op!(writer, I64EXTENDSI32),
			I64ExtendUI32 => op!(writer, I64EXTENDUI32),
			I64TruncSF32 => op!(writer, I64TRUNCSF32),
			I64TruncUF32 => op!(writer, I64TRUNCUF32),
			I64TruncSF64 => op!(writer, I64TRUNCSF64),
			I64TruncUF64 => op!(writer, I64TRUNCUF64),
			F32ConvertSI32 => op!(writer, F32CONVERTSI32),
			F32ConvertUI32 => op!(writer, F32CONVERTUI32),
			F32ConvertSI64 => op!(writer, F32CONVERTSI64),
			F32ConvertUI64 => op!(writer, F32CONVERTUI64),
			F32DemoteF64 => op!(writer, F32DEMOTEF64),
			F64ConvertSI32 => op!(writer, F64CONVERTSI32),
			F64ConvertUI32 => op!(writer, F64CONVERTUI32),
			F64ConvertSI64 => op!(writer, F64CONVERTSI64),
			F64ConvertUI64 => op!(writer, F64CONVERTUI64),
			F64PromoteF32 => op!(writer, F64PROMOTEF32),

			I32ReinterpretF32 => op!(writer, I32REINTERPRETF32),
			I64ReinterpretF64 => op!(writer, I64REINTERPRETF64),
			F32ReinterpretI32 => op!(writer, F32REINTERPRETI32),
			F64ReinterpretI64 => op!(writer, F64REINTERPRETI64),
        }
	}
}

#[allow(missing_docs)]
pub mod opcodes {
	pub const UNREACHABLE: u8 = 0x00;
	pub const NOP: u8 = 0x01;
	pub const BLOCK: u8 = 0x02;
	pub const LOOP: u8 = 0x03;
	pub const IF: u8 = 0x04;
	pub const ELSE: u8 = 0x05;
	pub const END: u8 = 0x0b;
	pub const BR: u8 = 0x0c;
	pub const BRIF: u8 = 0x0d;
	pub const BRTABLE: u8 = 0x0e;
	pub const RETURN: u8 = 0x0f;
	pub const CALL: u8 = 0x10;
	pub const CALLINDIRECT: u8 = 0x11;
	pub const DROP: u8 = 0x1a;
	pub const SELECT: u8 = 0x1b;
	pub const GETLOCAL: u8 = 0x20;
	pub const SETLOCAL: u8 = 0x21;
	pub const TEELOCAL: u8 = 0x22;
	pub const GETGLOBAL: u8 = 0x23;
	pub const SETGLOBAL: u8 = 0x24;
	pub const I32LOAD: u8 = 0x28;
	pub const I64LOAD: u8 = 0x29;
	pub const F32LOAD: u8 = 0x2a;
	pub const F64LOAD: u8 = 0x2b;
	pub const I32LOAD8S: u8 = 0x2c;
	pub const I32LOAD8U: u8 = 0x2d;
	pub const I32LOAD16S: u8 = 0x2e;
	pub const I32LOAD16U: u8 = 0x2f;
	pub const I64LOAD8S: u8 = 0x30;
	pub const I64LOAD8U: u8 = 0x31;
	pub const I64LOAD16S: u8 = 0x32;
	pub const I64LOAD16U: u8 = 0x33;
	pub const I64LOAD32S: u8 = 0x34;
	pub const I64LOAD32U: u8 = 0x35;
	pub const I32STORE: u8 = 0x36;
	pub const I64STORE: u8 = 0x37;
	pub const F32STORE: u8 = 0x38;
	pub const F64STORE: u8 = 0x39;
	pub const I32STORE8: u8 = 0x3a;
	pub const I32STORE16: u8 = 0x3b;
	pub const I64STORE8: u8 = 0x3c;
	pub const I64STORE16: u8 = 0x3d;
	pub const I64STORE32: u8 = 0x3e;
	pub const CURRENTMEMORY: u8 = 0x3f;
	pub const GROWMEMORY: u8 = 0x40;
	pub const I32CONST: u8 = 0x41;
	pub const I64CONST: u8 = 0x42;
	pub const F32CONST: u8 = 0x43;
	pub const F64CONST: u8 = 0x44;
	pub const I32EQZ: u8 = 0x45;
	pub const I32EQ: u8 = 0x46;
	pub const I32NE: u8 = 0x47;
	pub const I32LTS: u8 = 0x48;
	pub const I32LTU: u8 = 0x49;
	pub const I32GTS: u8 = 0x4a;
	pub const I32GTU: u8 = 0x4b;
	pub const I32LES: u8 = 0x4c;
	pub const I32LEU: u8 = 0x4d;
	pub const I32GES: u8 = 0x4e;
	pub const I32GEU: u8 = 0x4f;
	pub const I64EQZ: u8 = 0x50;
	pub const I64EQ: u8 = 0x51;
	pub const I64NE: u8 = 0x52;
	pub const I64LTS: u8 = 0x53;
	pub const I64LTU: u8 = 0x54;
	pub const I64GTS: u8 = 0x55;
	pub const I64GTU: u8 = 0x56;
	pub const I64LES: u8 = 0x57;
	pub const I64LEU: u8 = 0x58;
	pub const I64GES: u8 = 0x59;
    pub const I64GEU: u8 = 0x5a;
    
	pub const F32EQ: u8 = 0x5b;
	pub const F32NE: u8 = 0x5c;
	pub const F32LT: u8 = 0x5d;
	pub const F32GT: u8 = 0x5e;
	pub const F32LE: u8 = 0x5f;
	pub const F32GE: u8 = 0x60;

	pub const F64EQ: u8 = 0x61;
	pub const F64NE: u8 = 0x62;
	pub const F64LT: u8 = 0x63;
	pub const F64GT: u8 = 0x64;
	pub const F64LE: u8 = 0x65;
	pub const F64GE: u8 = 0x66;

	pub const I32CLZ: u8 = 0x67;
	pub const I32CTZ: u8 = 0x68;
	pub const I32POPCNT: u8 = 0x69;
	pub const I32ADD: u8 = 0x6a;
	pub const I32SUB: u8 = 0x6b;
	pub const I32MUL: u8 = 0x6c;
	pub const I32DIVS: u8 = 0x6d;
	pub const I32DIVU: u8 = 0x6e;
	pub const I32REMS: u8 = 0x6f;
	pub const I32REMU: u8 = 0x70;
	pub const I32AND: u8 = 0x71;
	pub const I32OR: u8 = 0x72;
	pub const I32XOR: u8 = 0x73;
	pub const I32SHL: u8 = 0x74;
	pub const I32SHRS: u8 = 0x75;
	pub const I32SHRU: u8 = 0x76;
	pub const I32ROTL: u8 = 0x77;
	pub const I32ROTR: u8 = 0x78;

	pub const I64CLZ: u8 = 0x79;
	pub const I64CTZ: u8 = 0x7a;
	pub const I64POPCNT: u8 = 0x7b;
	pub const I64ADD: u8 = 0x7c;
	pub const I64SUB: u8 = 0x7d;
	pub const I64MUL: u8 = 0x7e;
	pub const I64DIVS: u8 = 0x7f;
	pub const I64DIVU: u8 = 0x80;
	pub const I64REMS: u8 = 0x81;
	pub const I64REMU: u8 = 0x82;
	pub const I64AND: u8 = 0x83;
	pub const I64OR: u8 = 0x84;
	pub const I64XOR: u8 = 0x85;
	pub const I64SHL: u8 = 0x86;
	pub const I64SHRS: u8 = 0x87;
	pub const I64SHRU: u8 = 0x88;
	pub const I64ROTL: u8 = 0x89;
	pub const I64ROTR: u8 = 0x8a;
	pub const F32ABS: u8 = 0x8b;
	pub const F32NEG: u8 = 0x8c;
	pub const F32CEIL: u8 = 0x8d;
	pub const F32FLOOR: u8 = 0x8e;
	pub const F32TRUNC: u8 = 0x8f;
	pub const F32NEAREST: u8 = 0x90;
	pub const F32SQRT: u8 = 0x91;
	pub const F32ADD: u8 = 0x92;
	pub const F32SUB: u8 = 0x93;
	pub const F32MUL: u8 = 0x94;
	pub const F32DIV: u8 = 0x95;
	pub const F32MIN: u8 = 0x96;
	pub const F32MAX: u8 = 0x97;
	pub const F32COPYSIGN: u8 = 0x98;
	pub const F64ABS: u8 = 0x99;
	pub const F64NEG: u8 = 0x9a;
	pub const F64CEIL: u8 = 0x9b;
	pub const F64FLOOR: u8 = 0x9c;
	pub const F64TRUNC: u8 = 0x9d;
	pub const F64NEAREST: u8 = 0x9e;
	pub const F64SQRT: u8 = 0x9f;
	pub const F64ADD: u8 = 0xa0;
	pub const F64SUB: u8 = 0xa1;
	pub const F64MUL: u8 = 0xa2;
	pub const F64DIV: u8 = 0xa3;
	pub const F64MIN: u8 = 0xa4;
	pub const F64MAX: u8 = 0xa5;
	pub const F64COPYSIGN: u8 = 0xa6;

	pub const I32WRAPI64: u8 = 0xa7;
	pub const I32TRUNCSF32: u8 = 0xa8;
	pub const I32TRUNCUF32: u8 = 0xa9;
	pub const I32TRUNCSF64: u8 = 0xaa;
	pub const I32TRUNCUF64: u8 = 0xab;
	pub const I64EXTENDSI32: u8 = 0xac;
	pub const I64EXTENDUI32: u8 = 0xad;
	pub const I64TRUNCSF32: u8 = 0xae;
	pub const I64TRUNCUF32: u8 = 0xaf;
	pub const I64TRUNCSF64: u8 = 0xb0;
	pub const I64TRUNCUF64: u8 = 0xb1;
	pub const F32CONVERTSI32: u8 = 0xb2;
	pub const F32CONVERTUI32: u8 = 0xb3;
	pub const F32CONVERTSI64: u8 = 0xb4;
	pub const F32CONVERTUI64: u8 = 0xb5;
	pub const F32DEMOTEF64: u8 = 0xb6;
	pub const F64CONVERTSI32: u8 = 0xb7;
	pub const F64CONVERTUI32: u8 = 0xb8;
	pub const F64CONVERTSI64: u8 = 0xb9;
	pub const F64CONVERTUI64: u8 = 0xba;
	pub const F64PROMOTEF32: u8 = 0xbb;

	pub const I32REINTERPRETF32: u8 = 0xbc;
	pub const I64REINTERPRETF64: u8 = 0xbd;
	pub const F32REINTERPRETI32: u8 = 0xbe;
	pub const F64REINTERPRETI64: u8 = 0xbf;

	#[cfg(feature="sign_ext")]
	pub mod sign_ext {
		pub const I32_EXTEND8_S: u8 = 0xc0;
		pub const I32_EXTEND16_S: u8 = 0xc1;
		pub const I64_EXTEND8_S: u8 = 0xc2;
		pub const I64_EXTEND16_S: u8 = 0xc3;
		pub const I64_EXTEND32_S: u8 = 0xc4;
	}

	#[cfg(feature="atomics")]
	pub mod atomics {
		pub const ATOMIC_PREFIX: u8 = 0xfe;
		pub const ATOMIC_WAKE: u8 = 0x00;
		pub const I32_ATOMIC_WAIT: u8 = 0x01;
		pub const I64_ATOMIC_WAIT: u8 = 0x02;

		pub const I32_ATOMIC_LOAD: u8 = 0x10;
		pub const I64_ATOMIC_LOAD: u8 = 0x11;
		pub const I32_ATOMIC_LOAD8U: u8 = 0x12;
		pub const I32_ATOMIC_LOAD16U: u8 = 0x13;
		pub const I64_ATOMIC_LOAD8U: u8 = 0x14;
		pub const I64_ATOMIC_LOAD16U: u8 = 0x15;
		pub const I64_ATOMIC_LOAD32U: u8 = 0x16;
		pub const I32_ATOMIC_STORE: u8 = 0x17;
		pub const I64_ATOMIC_STORE: u8 = 0x18;
		pub const I32_ATOMIC_STORE8U: u8 = 0x19;
		pub const I32_ATOMIC_STORE16U: u8 = 0x1a;
		pub const I64_ATOMIC_STORE8U: u8 = 0x1b;
		pub const I64_ATOMIC_STORE16U: u8 = 0x1c;
		pub const I64_ATOMIC_STORE32U: u8 = 0x1d;

		pub const I32_ATOMIC_RMW_ADD: u8 = 0x1e;
		pub const I64_ATOMIC_RMW_ADD: u8 = 0x1f;
		pub const I32_ATOMIC_RMW_ADD8U: u8 = 0x20;
		pub const I32_ATOMIC_RMW_ADD16U: u8 = 0x21;
		pub const I64_ATOMIC_RMW_ADD8U: u8 = 0x22;
		pub const I64_ATOMIC_RMW_ADD16U: u8 = 0x23;
		pub const I64_ATOMIC_RMW_ADD32U: u8 = 0x24;

		pub const I32_ATOMIC_RMW_SUB: u8 = 0x25;
		pub const I64_ATOMIC_RMW_SUB: u8 = 0x26;
		pub const I32_ATOMIC_RMW_SUB8U: u8 = 0x27;
		pub const I32_ATOMIC_RMW_SUB16U: u8 = 0x28;
		pub const I64_ATOMIC_RMW_SUB8U: u8 = 0x29;
		pub const I64_ATOMIC_RMW_SUB16U: u8 = 0x2a;
		pub const I64_ATOMIC_RMW_SUB32U: u8 = 0x2b;

		pub const I32_ATOMIC_RMW_AND: u8 = 0x2c;
		pub const I64_ATOMIC_RMW_AND: u8 = 0x2d;
		pub const I32_ATOMIC_RMW_AND8U: u8 = 0x2e;
		pub const I32_ATOMIC_RMW_AND16U: u8 = 0x2f;
		pub const I64_ATOMIC_RMW_AND8U: u8 = 0x30;
		pub const I64_ATOMIC_RMW_AND16U: u8 = 0x31;
		pub const I64_ATOMIC_RMW_AND32U: u8 = 0x32;

		pub const I32_ATOMIC_RMW_OR: u8 = 0x33;
		pub const I64_ATOMIC_RMW_OR: u8 = 0x34;
		pub const I32_ATOMIC_RMW_OR8U: u8 = 0x35;
        pub const I32_ATOMIC_RMW_OR16U: u8 = 0x36;
        pub const I64_ATOMIC_RMW_OR8U: u8 = 0x37;
		pub const I64_ATOMIC_RMW_OR16U: u8 = 0x38;
		pub const I64_ATOMIC_RMW_OR32U: u8 = 0x39;

		pub const I32_ATOMIC_RMW_XOR: u8 = 0x3a;
		pub const I64_ATOMIC_RMW_XOR: u8 = 0x3b;
		pub const I32_ATOMIC_RMW_XOR8U: u8 = 0x3c;
		pub const I32_ATOMIC_RMW_XOR16U: u8 = 0x3d;
		pub const I64_ATOMIC_RMW_XOR8U: u8 = 0x3e;
		pub const I64_ATOMIC_RMW_XOR16U: u8 = 0x3f;
		pub const I64_ATOMIC_RMW_XOR32U: u8 = 0x40;

		pub const I32_ATOMIC_RMW_XCHG: u8 = 0x41;
		pub const I64_ATOMIC_RMW_XCHG: u8 = 0x42;
		pub const I32_ATOMIC_RMW_XCHG8U: u8 = 0x43;
		pub const I32_ATOMIC_RMW_XCHG16U: u8 = 0x44;
		pub const I64_ATOMIC_RMW_XCHG8U: u8 = 0x45;
		pub const I64_ATOMIC_RMW_XCHG16U: u8 = 0x46;
		pub const I64_ATOMIC_RMW_XCHG32U: u8 = 0x47;

		pub const I32_ATOMIC_RMW_CMPXCHG: u8 = 0x48;
		pub const I64_ATOMIC_RMW_CMPXCHG: u8 = 0x49;
		pub const I32_ATOMIC_RMW_CMPXCHG8U: u8 = 0x4a;
		pub const I32_ATOMIC_RMW_CMPXCHG16U: u8 = 0x4b;
		pub const I64_ATOMIC_RMW_CMPXCHG8U: u8 = 0x4c;
		pub const I64_ATOMIC_RMW_CMPXCHG16U: u8 = 0x4d;
		pub const I64_ATOMIC_RMW_CMPXCHG32U: u8 = 0x4e;
	}

	#[cfg(feature="simd")]
	pub mod simd {
		// https://github.com/WebAssembly/simd/blob/master/proposals/simd/BinarySIMD.md
		pub const SIMD_PREFIX: u8 = 0xfd;

		pub const V128_LOAD: u32 = 0x00;
		pub const V128_STORE: u32 = 0x01;
		pub const V128_CONST: u32 = 0x02;
		pub const V8X16_SHUFFLE: u32 = 0x03;

		pub const I8X16_SPLAT: u32 = 0x04;
		pub const I8X16_EXTRACT_LANE_S: u32 = 0x05;
		pub const I8X16_EXTRACT_LANE_U: u32 = 0x06;
		pub const I8X16_REPLACE_LANE: u32 = 0x07;
		pub const I16X8_SPLAT: u32 = 0x08;
		pub const I16X8_EXTRACT_LANE_S: u32 = 0x09;
		pub const I16X8_EXTRACT_LANE_U: u32 = 0xa;
		pub const I16X8_REPLACE_LANE: u32 = 0x0b;
		pub const I32X4_SPLAT: u32 = 0x0c;
		pub const I32X4_EXTRACT_LANE: u32 = 0x0d;
		pub const I32X4_REPLACE_LANE: u32 = 0x0e;
		pub const I64X2_SPLAT: u32 = 0x0f;
		pub const I64X2_EXTRACT_LANE: u32 = 0x10;
		pub const I64X2_REPLACE_LANE: u32 = 0x11;
		pub const F32X4_SPLAT: u32 = 0x12;
		pub const F32X4_EXTRACT_LANE: u32 = 0x13;
		pub const F32X4_REPLACE_LANE: u32 = 0x14;
		pub const F64X2_SPLAT: u32 = 0x15;
		pub const F64X2_EXTRACT_LANE: u32 = 0x16;
		pub const F64X2_REPLACE_LANE: u32 = 0x17;

		pub const I8X16_EQ: u32 = 0x18;
		pub const I8X16_NE: u32 = 0x19;
		pub const I8X16_LT_S: u32 = 0x1a;
		pub const I8X16_LT_U: u32 = 0x1b;
		pub const I8X16_GT_S: u32 = 0x1c;
		pub const I8X16_GT_U: u32 = 0x1d;
		pub const I8X16_LE_S: u32 = 0x1e;
		pub const I8X16_LE_U: u32 = 0x1f;
		pub const I8X16_GE_S: u32 = 0x20;
		pub const I8X16_GE_U: u32 = 0x21;

		pub const I16X8_EQ: u32 = 0x22;
		pub const I16X8_NE: u32 = 0x23;
		pub const I16X8_LT_S: u32 = 0x24;
		pub const I16X8_LT_U: u32 = 0x25;
		pub const I16X8_GT_S: u32 = 0x26;
		pub const I16X8_GT_U: u32 = 0x27;
		pub const I16X8_LE_S: u32 = 0x28;
		pub const I16X8_LE_U: u32 = 0x29;
		pub const I16X8_GE_S: u32 = 0x2a;
		pub const I16X8_GE_U: u32 = 0x2b;

		pub const I32X4_EQ: u32 = 0x2c;
		pub const I32X4_NE: u32 = 0x2d;
		pub const I32X4_LT_S: u32 = 0x2e;
		pub const I32X4_LT_U: u32 = 0x2f;
		pub const I32X4_GT_S: u32 = 0x30;
		pub const I32X4_GT_U: u32 = 0x31;
		pub const I32X4_LE_S: u32 = 0x32;
		pub const I32X4_LE_U: u32 = 0x33;
		pub const I32X4_GE_S: u32 = 0x34;
		pub const I32X4_GE_U: u32 = 0x35;

		pub const F32X4_EQ: u32 = 0x40;
		pub const F32X4_NE: u32 = 0x41;
		pub const F32X4_LT: u32 = 0x42;
		pub const F32X4_GT: u32 = 0x43;
		pub const F32X4_LE: u32 = 0x44;
		pub const F32X4_GE: u32 = 0x45;

		pub const F64X2_EQ: u32 = 0x46;
		pub const F64X2_NE: u32 = 0x47;
		pub const F64X2_LT: u32 = 0x48;
		pub const F64X2_GT: u32 = 0x49;
		pub const F64X2_LE: u32 = 0x4a;
		pub const F64X2_GE: u32 = 0x4b;

		pub const V128_NOT: u32 = 0x4c;
		pub const V128_AND: u32 = 0x4d;
		pub const V128_OR: u32 = 0x4e;
		pub const V128_XOR: u32 = 0x4f;
		pub const V128_BITSELECT: u32 = 0x50;

		pub const I8X16_NEG: u32 = 0x51;
		pub const I8X16_ANY_TRUE: u32 = 0x52;
		pub const I8X16_ALL_TRUE: u32 = 0x53;
		pub const I8X16_SHL: u32 = 0x54;
		pub const I8X16_SHR_S: u32 = 0x55;
		pub const I8X16_SHR_U: u32 = 0x56;
		pub const I8X16_ADD: u32 = 0x57;
		pub const I8X16_ADD_SATURATE_S: u32 = 0x58;
		pub const I8X16_ADD_SATURATE_U: u32 = 0x59;
		pub const I8X16_SUB: u32 = 0x5a;
		pub const I8X16_SUB_SATURATE_S: u32 = 0x5b;
		pub const I8X16_SUB_SATURATE_U: u32 = 0x5c;
		pub const I8X16_MUL: u32 = 0x5d;

		pub const I16X8_NEG: u32 = 0x62;
		pub const I16X8_ANY_TRUE: u32 = 0x63;
		pub const I16X8_ALL_TRUE: u32 = 0x64;
		pub const I16X8_SHL: u32 = 0x65;
		pub const I16X8_SHR_S: u32 = 0x66;
		pub const I16X8_SHR_U: u32 = 0x67;
		pub const I16X8_ADD: u32 = 0x68;
		pub const I16X8_ADD_SATURATE_S: u32 = 0x69;
		pub const I16X8_ADD_SATURATE_U: u32 = 0x6a;
		pub const I16X8_SUB: u32 = 0x6b;
		pub const I16X8_SUB_SATURATE_S: u32 = 0x6c;
		pub const I16X8_SUB_SATURATE_U: u32 = 0x6d;
		pub const I16X8_MUL: u32 = 0x6e;

		pub const I32X4_NEG: u32 = 0x73;
		pub const I32X4_ANY_TRUE: u32 = 0x74;
		pub const I32X4_ALL_TRUE: u32 = 0x75;
		pub const I32X4_SHL: u32 = 0x76;
		pub const I32X4_SHR_S: u32 = 0x77;
		pub const I32X4_SHR_U: u32 = 0x78;
		pub const I32X4_ADD: u32 = 0x79;
		pub const I32X4_ADD_SATURATE_S: u32 = 0x7a;
		pub const I32X4_ADD_SATURATE_U: u32 = 0x7b;
		pub const I32X4_SUB: u32 = 0x7c;
		pub const I32X4_SUB_SATURATE_S: u32 = 0x7d;
		pub const I32X4_SUB_SATURATE_U: u32 = 0x7e;
		pub const I32X4_MUL: u32 = 0x7f;

		pub const I64X2_NEG: u32 = 0x84;
		pub const I64X2_ANY_TRUE: u32 = 0x85;
		pub const I64X2_ALL_TRUE: u32 = 0x86;
		pub const I64X2_SHL: u32 = 0x87;
		pub const I64X2_SHR_S: u32 = 0x88;
		pub const I64X2_SHR_U: u32 = 0x89;
		pub const I64X2_ADD: u32 = 0x8a;
		pub const I64X2_SUB: u32 = 0x8d;

		pub const F32X4_ABS: u32 = 0x95;
		pub const F32X4_NEG: u32 = 0x96;
		pub const F32X4_SQRT: u32 = 0x97;
		pub const F32X4_ADD: u32 = 0x9a;
		pub const F32X4_SUB: u32 = 0x9b;
		pub const F32X4_MUL: u32 = 0x9c;
		pub const F32X4_DIV: u32 = 0x9d;
		pub const F32X4_MIN: u32 = 0x9e;
		pub const F32X4_MAX: u32 = 0x9f;

		pub const F64X2_ABS: u32 = 0xa0;
		pub const F64X2_NEG: u32 = 0xa1;
		pub const F64X2_SQRT: u32 = 0xa2;
		pub const F64X2_ADD: u32 = 0xa5;
		pub const F64X2_SUB: u32 = 0xa6;
		pub const F64X2_MUL: u32 = 0xa7;
		pub const F64X2_DIV: u32 = 0xa8;
		pub const F64X2_MIN: u32 = 0xa9;
		pub const F64X2_MAX: u32 = 0xaa;

		pub const I32X4_TRUNC_S_F32X4_SAT: u32 = 0xab;
		pub const I32X4_TRUNC_U_F32X4_SAT: u32 = 0xac;
		pub const I64X2_TRUNC_S_F64X2_SAT: u32 = 0xad;
		pub const I64X2_TRUNC_U_F64X2_SAT: u32 = 0xae;

		pub const F32X4_CONVERT_S_I32X4: u32 = 0xaf;
		pub const F32X4_CONVERT_U_I32X4: u32 = 0xb0;
		pub const F64X2_CONVERT_S_I64X2: u32 = 0xb1;
		pub const F64X2_CONVERT_U_I64X2: u32 = 0xb2;
	}

	#[cfg(feature="bulk")]
	pub mod bulk {
		pub const BULK_PREFIX: u8 = 0xfc;
		pub const MEMORY_INIT: u8 = 0x08;
		pub const MEMORY_DROP: u8 = 0x09;
		pub const MEMORY_COPY: u8 = 0x0a;
		pub const MEMORY_FILL: u8 = 0x0b;
		pub const TABLE_INIT: u8 = 0x0c;
		pub const TABLE_DROP: u8 = 0x0d;
		pub const TABLE_COPY: u8 = 0x0e;
	}
}
