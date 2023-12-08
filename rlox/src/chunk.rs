use std::ffi::{c_int, c_uint};
use std::fmt;

use crate::alloc::Gc;
use crate::object::ObjFunction;
use crate::value::Value;
use crate::vec::Vec;

macro_rules! delegate {
    ($Src:ty, $Tgt:ty) => {
        impl std::ops::Deref for $Src {
            type Target = $Tgt;
            fn deref(&self) -> &$Tgt {
                &self.0
            }
        }

        impl std::ops::DerefMut for $Src {
            fn deref_mut(&mut self) -> &mut $Tgt {
                &mut self.0
            }
        }
    };
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Bytecode(Vec<u8>);

delegate!(Bytecode, Vec<u8>);

#[derive(Debug)]
#[repr(transparent)]
pub struct Lines(Vec<c_int>);

delegate!(Lines, Vec<c_int>);

#[derive(Debug)]
#[repr(transparent)]
pub struct Values(Vec<Value>);

delegate!(Values, Vec<Value>);

#[repr(C)]
pub struct Chunk {
    pub(crate) code: Bytecode,
    pub(crate) constants: Values,
    pub(crate) lines: Lines,
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chunk")
            .field("code", &format!("({} bytes)", self.code.len()))
            .field(
                "constants",
                &format!("({} constants)", self.constants.len()),
            )
            .finish()
    }
}

impl Chunk {
    pub fn init(&mut self) {
        self.code.init();
        self.constants.init();
        self.lines.init();
    }

    pub fn free(&mut self, gc: &mut Gc) {
        self.code.free(gc);
        self.constants.free(gc);
        self.lines.free(gc);
    }

    pub fn write_byte(&mut self, gc: Gc, byte: u8, line: c_int) {
        self.code.push(gc, byte);
        self.lines.push(gc, line);
    }

    pub fn add_constant(&mut self, gc: Gc, value: Value) -> u8 {
        let idx = self.constants.len();

        // GC: Ensure `value` is reachable temporarily in case resizing the constants
        // array triggers garbage collection.
        //
        // TODO: Use Alloc stash
        unsafe { gc.vm.as_mut().unwrap() }.push(value);
        self.constants.push(gc, value);
        unsafe { gc.vm.as_mut().unwrap() }.pop();

        idx as u8
    }
}

impl Chunk {
    pub(crate) fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        for i in 0..self.constants.len() {
            print!("CONSTANT {:04} = ", i);

            let v = self.constants.get(i);
            v.print();

            println!();
        }

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub(crate) fn disassemble_instruction(&self, offset: c_uint) -> c_uint {
        print!("{:04} ", offset);

        if offset > 0 && self.lines.get(offset) == self.lines.get(offset - 1) {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines.get(offset));
        }

        let opcode = *self.code.get(offset);
        let Ok(opcode) = opcode.try_into() else {
            println!("Unknown opcode: {}", opcode);
            return offset + 1;
        };

        match opcode {
            op @ (Opcode::OpConstant
            | Opcode::OpClass
            | Opcode::OpMethod
            | Opcode::OpDefineGlobal
            | Opcode::OpGetGlobal
            | Opcode::OpSetGlobal
            | Opcode::OpGetProperty
            | Opcode::OpSetProperty
            | Opcode::OpGetSuper) => self.constant_instruction(op, offset),

            op @ (Opcode::OpGetLocal
            | Opcode::OpSetLocal
            | Opcode::OpGetUpvalue
            | Opcode::OpSetUpvalue
            | Opcode::OpCall) => self.byte_instruction(op, offset),

            op @ (Opcode::OpJump | Opcode::OpJumpIfFalse) => self.jump_instruction(op, offset),
            op @ Opcode::OpLoop => self.loop_instruction(op, offset),

            op @ (Opcode::OpNil
            | Opcode::OpTrue
            | Opcode::OpFalse
            | Opcode::OpPop
            | Opcode::OpEqual
            | Opcode::OpGreater
            | Opcode::OpLess
            | Opcode::OpNot
            | Opcode::OpNeg
            | Opcode::OpAdd
            | Opcode::OpSub
            | Opcode::OpMul
            | Opcode::OpDiv
            | Opcode::OpPrint
            | Opcode::OpCloseUpvalue
            | Opcode::OpReturn
            | Opcode::OpInherit) => self.simple_instruction(op, offset),

            op @ (Opcode::OpInvoke | Opcode::OpSuperInvoke) => self.invoke_instruction(op, offset),

            op @ Opcode::OpClosure => self.closure_instruction(op, offset),
        }
    }

    fn simple_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        println!("{: <16?}", op);
        offset + 1
    }

    fn byte_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        let slot = self.code.get(offset + 1);
        println!("{: <16?} {:04}", op, slot);
        offset + 2
    }

    fn constant_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        let constant = *self.code.get(offset + 1);
        print!("{: <16?} {:04} '", op, constant);

        let v = self.constants.get(constant.into());
        v.print();

        println!("'");
        offset + 2
    }

    fn jump_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        let hi: u16 = (*self.code.get(offset + 1)).into();
        let lo: u16 = (*self.code.get(offset + 2)).into();
        let target = offset + 3 + ((hi << 8 | lo) as c_uint);

        println!("{: <16?} {:04} -> {}", op, offset, target);
        offset + 3
    }

    fn loop_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        let hi: u16 = (*self.code.get(offset + 1)).into();
        let lo: u16 = (*self.code.get(offset + 2)).into();
        let target = offset + 3 - ((hi << 8 | lo) as c_uint);

        println!("{: <16?} {:04} -> {}", op, offset, target);
        offset + 3
    }

    fn invoke_instruction(&self, op: Opcode, offset: c_uint) -> c_uint {
        let constant = *self.code.get(offset + 1);
        let argc = *self.code.get(offset + 2);

        print!("{: <16?} ({:04} args) {:04} ", op, argc, constant);

        let v = self.constants.get(constant.into());
        v.print();

        println!();
        offset + 3
    }

    fn closure_instruction(&self, op: Opcode, mut offset: c_uint) -> c_uint {
        offset += 1;

        let constant = *self.code.get(offset + 1);
        print!("{: <16?} {:04} ", op, constant);

        let v = self.constants.get(constant.into());
        v.print();

        println!();

        let function = unsafe { self.constants.get(constant.into()).as_obj::<ObjFunction>() };
        let upvalue_count = unsafe { function.as_ref().unwrap() }.upvalue_count;
        for _ in 0..upvalue_count {
            let is_local = *self.code.get(offset);
            offset += 1;

            let index = *self.code.get(offset);
            offset += 1;

            let ty = if is_local == 0 { "upvalue " } else { "local" };
            println!(
                "{:04}      |                     {} {}",
                offset - 2,
                ty,
                index
            );
        }

        offset
    }
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub enum Opcode {
    OpConstant,

    OpNil,
    OpTrue,
    OpFalse,

    OpPop,

    OpGetLocal,
    OpSetLocal,
    OpDefineGlobal,
    OpGetGlobal,
    OpSetGlobal,
    OpGetUpvalue,
    OpSetUpvalue,
    OpGetProperty,
    OpSetProperty,
    OpGetSuper,

    OpNot,
    OpEqual,
    OpGreater,
    OpLess,
    // TODO: The other three _are_ needed to handle NaN properly.
    OpNeg,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,

    OpPrint,

    OpJump,
    OpJumpIfFalse,
    OpLoop,

    OpCall,
    OpInvoke,
    OpSuperInvoke,
    OpClosure,
    OpCloseUpvalue,
    OpReturn,

    OpClass,
    OpInherit,
    OpMethod,
}

pub struct InvalidOpcode {
    pub opcode: u8,
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

impl TryFrom<u8> for Opcode {
    type Error = InvalidOpcode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let v = match value {
            v if v == Opcode::OpConstant as u8 => Opcode::OpConstant,
            v if v == Opcode::OpNil as u8 => Opcode::OpNil,
            v if v == Opcode::OpTrue as u8 => Opcode::OpTrue,
            v if v == Opcode::OpFalse as u8 => Opcode::OpFalse,

            v if v == Opcode::OpPop as u8 => Opcode::OpPop,

            v if v == Opcode::OpGetLocal as u8 => Opcode::OpGetLocal,
            v if v == Opcode::OpSetLocal as u8 => Opcode::OpSetLocal,
            v if v == Opcode::OpDefineGlobal as u8 => Opcode::OpDefineGlobal,
            v if v == Opcode::OpGetGlobal as u8 => Opcode::OpGetGlobal,
            v if v == Opcode::OpSetGlobal as u8 => Opcode::OpSetGlobal,
            v if v == Opcode::OpGetUpvalue as u8 => Opcode::OpGetUpvalue,
            v if v == Opcode::OpSetUpvalue as u8 => Opcode::OpSetUpvalue,
            v if v == Opcode::OpGetProperty as u8 => Opcode::OpGetProperty,
            v if v == Opcode::OpSetProperty as u8 => Opcode::OpSetProperty,
            v if v == Opcode::OpGetSuper as u8 => Opcode::OpGetSuper,

            v if v == Opcode::OpNot as u8 => Opcode::OpNot,
            v if v == Opcode::OpEqual as u8 => Opcode::OpEqual,
            v if v == Opcode::OpGreater as u8 => Opcode::OpGreater,
            v if v == Opcode::OpLess as u8 => Opcode::OpLess,

            v if v == Opcode::OpNeg as u8 => Opcode::OpNeg,
            v if v == Opcode::OpAdd as u8 => Opcode::OpAdd,
            v if v == Opcode::OpSub as u8 => Opcode::OpSub,
            v if v == Opcode::OpMul as u8 => Opcode::OpMul,
            v if v == Opcode::OpDiv as u8 => Opcode::OpDiv,

            v if v == Opcode::OpPrint as u8 => Opcode::OpPrint,

            v if v == Opcode::OpJump as u8 => Opcode::OpJump,
            v if v == Opcode::OpJumpIfFalse as u8 => Opcode::OpJumpIfFalse,
            v if v == Opcode::OpLoop as u8 => Opcode::OpLoop,

            v if v == Opcode::OpCall as u8 => Opcode::OpCall,
            v if v == Opcode::OpInvoke as u8 => Opcode::OpInvoke,
            v if v == Opcode::OpSuperInvoke as u8 => Opcode::OpSuperInvoke,
            v if v == Opcode::OpClosure as u8 => Opcode::OpClosure,
            v if v == Opcode::OpCloseUpvalue as u8 => Opcode::OpCloseUpvalue,
            v if v == Opcode::OpReturn as u8 => Opcode::OpReturn,

            v if v == Opcode::OpClass as u8 => Opcode::OpClass,
            v if v == Opcode::OpInherit as u8 => Opcode::OpInherit,
            v if v == Opcode::OpMethod as u8 => Opcode::OpMethod,

            opcode => return Err(InvalidOpcode { opcode }),
        };

        Ok(v)
    }
}
