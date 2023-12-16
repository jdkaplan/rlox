use std::fmt;

use crate::alloc::Gc;
use crate::object::ObjFunction;
use crate::value::Value;

#[derive(Default)]
#[repr(C)]
pub struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    pub(crate) lines: Vec<usize>,
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
    pub fn write_byte(&mut self, _gc: Gc, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, gc: Gc, value: Value) -> u8 {
        let idx = self.constants.len();

        // GC: Ensure `value` is reachable temporarily in case resizing the constants
        // array triggers garbage collection.
        //
        // TODO: Use Alloc stash
        unsafe { gc.vm.as_mut().unwrap() }.push(value);
        self.constants.push(value);
        unsafe { gc.vm.as_mut().unwrap() }.pop();

        idx as u8
    }
}

impl Chunk {
    // This is only used for certain debug features. Prevent it from being considered dead code.
    #[allow(dead_code)]
    pub(crate) fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        for i in 0..self.constants.len() {
            println!("CONSTANT {:04} = {}", i, self.constants[i]);
        }

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub(crate) fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let opcode = self.code[offset];
        let Ok(opcode) = opcode.try_into() else {
            println!("Unknown opcode: {}", opcode);
            return offset + 1;
        };

        match opcode {
            op @ (Opcode::Constant
            | Opcode::Class
            | Opcode::Method
            | Opcode::DefineGlobal
            | Opcode::GetGlobal
            | Opcode::SetGlobal
            | Opcode::GetProperty
            | Opcode::SetProperty
            | Opcode::GetSuper) => self.constant_instruction(op, offset),

            op @ (Opcode::GetLocal
            | Opcode::SetLocal
            | Opcode::GetUpvalue
            | Opcode::SetUpvalue
            | Opcode::Call) => self.byte_instruction(op, offset),

            op @ (Opcode::Jump | Opcode::JumpIfFalse) => self.jump_instruction(op, offset),
            op @ Opcode::Loop => self.loop_instruction(op, offset),

            op @ (Opcode::Nil
            | Opcode::True
            | Opcode::False
            | Opcode::Pop
            | Opcode::Equal
            | Opcode::Greater
            | Opcode::Less
            | Opcode::Not
            | Opcode::Neg
            | Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::Print
            | Opcode::CloseUpvalue
            | Opcode::Return
            | Opcode::Inherit) => self.simple_instruction(op, offset),

            op @ (Opcode::Invoke | Opcode::SuperInvoke) => self.invoke_instruction(op, offset),

            op @ Opcode::Closure => self.closure_instruction(op, offset),
        }
    }

    fn simple_instruction(&self, op: Opcode, offset: usize) -> usize {
        println!("{: <16?}", op);
        offset + 1
    }

    fn byte_instruction(&self, op: Opcode, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{: <16?} {:04}", op, slot);
        offset + 2
    }

    fn constant_instruction(&self, op: Opcode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let val = self.constants[constant as usize];
        print!("{: <16?} {:04} '{}'", op, constant, val);
        offset + 2
    }

    fn jump_instruction(&self, op: Opcode, offset: usize) -> usize {
        let hi: u16 = self.code[offset + 1].into();
        let lo: u16 = self.code[offset + 2].into();
        let target = offset + 3 + ((hi << 8 | lo) as usize);

        println!("{: <16?} {:04} -> {}", op, offset, target);
        offset + 3
    }

    fn loop_instruction(&self, op: Opcode, offset: usize) -> usize {
        let hi: u16 = self.code[offset + 1].into();
        let lo: u16 = self.code[offset + 2].into();
        let target = offset + 3 - ((hi << 8 | lo) as usize);

        println!("{: <16?} {:04} -> {}", op, offset, target);
        offset + 3
    }

    fn invoke_instruction(&self, op: Opcode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let argc = self.code[offset + 2];
        let val = self.constants[constant as usize];

        println!("{: <16?} ({:04} args) {:04} {}", op, argc, constant, val);
        offset + 3
    }

    fn closure_instruction(&self, op: Opcode, mut offset: usize) -> usize {
        offset += 1;

        let constant = self.code[offset];
        offset += 1;

        let val = self.constants[constant as usize];
        println!("{: <16?} {:04} {}", op, constant, val);

        let function = unsafe { self.constants[constant as usize].as_obj::<ObjFunction>() };
        let upvalue_count = unsafe { function.as_ref().unwrap() }.upvalue_count;
        for _ in 0..upvalue_count {
            let is_local = self.code[offset];
            offset += 1;

            let index = self.code[offset];
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

#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub enum Opcode {
    Constant,

    Nil,
    True,
    False,

    Pop,

    GetLocal,
    SetLocal,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,

    Not,
    Equal,
    Greater,
    Less,
    // TODO: The other three _are_ needed to handle NaN properly.
    Neg,
    Add,
    Sub,
    Mul,
    Div,

    Print,

    Jump,
    JumpIfFalse,
    Loop,

    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,

    Class,
    Inherit,
    Method,
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
            v if v == Opcode::Constant as u8 => Opcode::Constant,
            v if v == Opcode::Nil as u8 => Opcode::Nil,
            v if v == Opcode::True as u8 => Opcode::True,
            v if v == Opcode::False as u8 => Opcode::False,

            v if v == Opcode::Pop as u8 => Opcode::Pop,

            v if v == Opcode::GetLocal as u8 => Opcode::GetLocal,
            v if v == Opcode::SetLocal as u8 => Opcode::SetLocal,
            v if v == Opcode::DefineGlobal as u8 => Opcode::DefineGlobal,
            v if v == Opcode::GetGlobal as u8 => Opcode::GetGlobal,
            v if v == Opcode::SetGlobal as u8 => Opcode::SetGlobal,
            v if v == Opcode::GetUpvalue as u8 => Opcode::GetUpvalue,
            v if v == Opcode::SetUpvalue as u8 => Opcode::SetUpvalue,
            v if v == Opcode::GetProperty as u8 => Opcode::GetProperty,
            v if v == Opcode::SetProperty as u8 => Opcode::SetProperty,
            v if v == Opcode::GetSuper as u8 => Opcode::GetSuper,

            v if v == Opcode::Not as u8 => Opcode::Not,
            v if v == Opcode::Equal as u8 => Opcode::Equal,
            v if v == Opcode::Greater as u8 => Opcode::Greater,
            v if v == Opcode::Less as u8 => Opcode::Less,

            v if v == Opcode::Neg as u8 => Opcode::Neg,
            v if v == Opcode::Add as u8 => Opcode::Add,
            v if v == Opcode::Sub as u8 => Opcode::Sub,
            v if v == Opcode::Mul as u8 => Opcode::Mul,
            v if v == Opcode::Div as u8 => Opcode::Div,

            v if v == Opcode::Print as u8 => Opcode::Print,

            v if v == Opcode::Jump as u8 => Opcode::Jump,
            v if v == Opcode::JumpIfFalse as u8 => Opcode::JumpIfFalse,
            v if v == Opcode::Loop as u8 => Opcode::Loop,

            v if v == Opcode::Call as u8 => Opcode::Call,
            v if v == Opcode::Invoke as u8 => Opcode::Invoke,
            v if v == Opcode::SuperInvoke as u8 => Opcode::SuperInvoke,
            v if v == Opcode::Closure as u8 => Opcode::Closure,
            v if v == Opcode::CloseUpvalue as u8 => Opcode::CloseUpvalue,
            v if v == Opcode::Return as u8 => Opcode::Return,

            v if v == Opcode::Class as u8 => Opcode::Class,
            v if v == Opcode::Inherit as u8 => Opcode::Inherit,
            v if v == Opcode::Method as u8 => Opcode::Method,

            opcode => return Err(InvalidOpcode { opcode }),
        };

        Ok(v)
    }
}
