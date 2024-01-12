use std::fmt;
use std::io;
use std::io::Write;

use crate::gc::Gc;
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

    pub fn add_constant(&mut self, mut gc: Gc, value: Value) -> u8 {
        let idx = self.constants.len();

        // GC: Ensure `value` is reachable temporarily in case resizing the constants
        // array triggers garbage collection.
        //
        // TODO: Use Alloc stash
        unsafe { gc.vm.as_mut() }.push(value);
        self.constants.push(value);
        unsafe { gc.vm.as_mut() }.pop();

        idx as u8
    }
}

impl Chunk {
    // This is only used for certain debug features. Prevent it from being considered dead code.
    #[allow(dead_code)]
    pub(crate) fn disassemble(&self, name: &str) -> io::Result<String> {
        let mut buf = Vec::new();

        writeln!(&mut buf, "== {} ==", name)?;

        for i in 0..self.constants.len() {
            writeln!(&mut buf, "CONSTANT {:04} = {}", i, self.constants[i])?;
        }

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset, &mut buf)?;
        }

        Ok(String::from_utf8(buf).unwrap())
    }

    pub(crate) fn disassemble_instruction(
        &self,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        write!(f, "{:04} ", offset)?;

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            write!(f, "   | ")?;
        } else {
            write!(f, "{:4} ", self.lines[offset])?;
        }

        let opcode = self.code[offset];
        let Ok(opcode) = opcode.try_into() else {
            writeln!(f, "Unknown opcode: {}", opcode)?;
            return Ok(offset + 1);
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
            | Opcode::GetSuper) => self.constant_instruction(op, offset, f),

            op @ (Opcode::GetLocal
            | Opcode::SetLocal
            | Opcode::GetUpvalue
            | Opcode::SetUpvalue
            | Opcode::Call) => self.byte_instruction(op, offset, f),

            op @ (Opcode::Jump | Opcode::JumpIfFalse | Opcode::JumpIfTrue) => {
                self.jump_instruction(op, offset, f)
            }
            op @ Opcode::Loop => self.loop_instruction(op, offset, f),

            op @ (Opcode::Nil
            | Opcode::True
            | Opcode::False
            | Opcode::Pop
            | Opcode::Eq
            | Opcode::Ne
            | Opcode::Gt
            | Opcode::Ge
            | Opcode::Lt
            | Opcode::Le
            | Opcode::Not
            | Opcode::Neg
            | Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::Rem
            | Opcode::Print
            | Opcode::CloseUpvalue
            | Opcode::Return
            | Opcode::Inherit) => self.simple_instruction(op, offset, f),

            op @ (Opcode::Invoke | Opcode::SuperInvoke) => self.invoke_instruction(op, offset, f),

            op @ Opcode::Closure => self.closure_instruction(op, offset, f),
        }
    }

    fn simple_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        writeln!(f, "{: <16?}", op)?;
        Ok(offset + 1)
    }

    fn byte_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        let slot = self.code[offset + 1];
        writeln!(f, "{: <16?} {:04}", op, slot)?;
        Ok(offset + 2)
    }

    fn constant_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        let constant = self.code[offset + 1];
        let val = self.constants[constant as usize];
        writeln!(f, "{: <16?} {:04} '{}'", op, constant, val)?;
        Ok(offset + 2)
    }

    fn jump_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        let hi: u16 = self.code[offset + 1].into();
        let lo: u16 = self.code[offset + 2].into();
        let target = offset + 3 + ((hi << 8 | lo) as usize);

        writeln!(f, "{: <16?} {:04} -> {}", op, offset, target)?;
        Ok(offset + 3)
    }

    fn loop_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        let hi: u16 = self.code[offset + 1].into();
        let lo: u16 = self.code[offset + 2].into();
        let target = offset + 3 - ((hi << 8 | lo) as usize);

        writeln!(f, "{: <16?} {:04} -> {}", op, offset, target)?;
        Ok(offset + 3)
    }

    fn invoke_instruction(
        &self,
        op: Opcode,
        offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        let constant = self.code[offset + 1];
        let argc = self.code[offset + 2];
        let val = self.constants[constant as usize];

        writeln!(f, "{: <16?} ({:04} args) {:04} {}", op, argc, constant, val)?;
        Ok(offset + 3)
    }

    fn closure_instruction(
        &self,
        op: Opcode,
        mut offset: usize,
        f: &mut dyn io::Write,
    ) -> io::Result<usize> {
        offset += 1;

        let constant = self.code[offset];
        offset += 1;

        let val = self.constants[constant as usize];
        writeln!(f, "{: <16?} {:04} {}", op, constant, val)?;

        let function = unsafe { self.constants[constant as usize].as_obj::<ObjFunction>() };
        let upvalue_count = unsafe { function.as_ref() }.upvalue_count;
        for _ in 0..upvalue_count {
            let is_local = self.code[offset];
            offset += 1;

            let index = self.code[offset];
            offset += 1;

            let ty = if is_local == 0 { "upvalue " } else { "local" };
            writeln!(
                f,
                "{:04}      |                     {} {}",
                offset - 2,
                ty,
                index
            )?;
        }

        Ok(offset)
    }
}

#[derive(Debug, PartialEq, Eq, strum::FromRepr)]
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
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Print,

    Jump,
    JumpIfFalse,
    JumpIfTrue,
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

    fn try_from(opcode: u8) -> Result<Self, Self::Error> {
        Self::from_repr(opcode as usize).ok_or(InvalidOpcode { opcode })
    }
}
