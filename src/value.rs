use std::fmt;

use crate::object::{Obj, ObjType};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Value {
    pub(crate) r#type: ValueType,
    pub(crate) r#as: ValueAs,
}

impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

impl Value {
    pub(crate) fn bool(boolean: bool) -> Self {
        Self {
            r#type: ValueType::Bool,
            r#as: ValueAs { boolean },
        }
    }

    pub(crate) fn nil() -> Self {
        Self {
            r#type: ValueType::Nil,
            r#as: ValueAs { number: 0.0 },
        }
    }

    pub(crate) fn number(number: f64) -> Self {
        Self {
            r#type: ValueType::Number,
            r#as: ValueAs { number },
        }
    }

    pub(crate) fn obj(obj: *mut Obj) -> Self {
        Self {
            r#type: ValueType::Obj,
            r#as: ValueAs { obj },
        }
    }
}

impl Value {
    pub(crate) fn is_bool(&self) -> bool {
        self.r#type == ValueType::Bool
    }

    pub(crate) fn is_nil(&self) -> bool {
        self.r#type == ValueType::Nil
    }

    pub(crate) fn is_number(&self) -> bool {
        self.r#type == ValueType::Number
    }

    pub(crate) fn is_obj(&self) -> bool {
        self.r#type == ValueType::Obj
    }

    pub(crate) fn is_falsey(&self) -> bool {
        self.is_nil() || (self.is_bool() && unsafe { !self.as_bool() })
    }
}

impl Value {
    pub(crate) unsafe fn as_bool(&self) -> bool {
        self.r#as.boolean
    }

    pub(crate) unsafe fn as_number(&self) -> f64 {
        self.r#as.number
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.r#type {
            ValueType::Bool => {
                if unsafe { self.r#as.boolean } {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            ValueType::Nil => write!(f, "nil"),
            ValueType::Number => write!(f, "{}", unsafe { self.r#as.number }),
            ValueType::Obj => write!(f, "{}", unsafe { self.r#as.obj.as_ref().unwrap() }),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Value")
            .field("r#type", &self.r#type)
            .field(
                "r#as",
                match self.r#type {
                    ValueType::Bool => unsafe { &self.r#as.boolean },
                    ValueType::Nil => &"nil",
                    ValueType::Number => unsafe { &self.r#as.number },
                    ValueType::Obj => unsafe { &self.r#as.obj },
                },
            )
            .finish()
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self.r#type, other.r#type) {
            (ValueType::Bool, ValueType::Bool) => unsafe {
                self.r#as.boolean == other.r#as.boolean
            },
            (ValueType::Bool, _) => false,

            (ValueType::Nil, ValueType::Nil) => true,
            (ValueType::Nil, _) => false,

            (ValueType::Number, ValueType::Number) => unsafe {
                self.r#as.number == other.r#as.number
            },
            (ValueType::Number, _) => false,

            // The VM interns all strings, so pointer equality works for every object.
            (ValueType::Obj, ValueType::Obj) => unsafe { self.r#as.obj == other.r#as.obj },
            (ValueType::Obj, _) => false,
        }
    }
}

impl Value {
    pub(crate) fn is_obj_type(&self, ty: ObjType) -> bool {
        self.r#type == ValueType::Obj && unsafe { self.r#as.obj.as_ref().unwrap() }.r#type == ty
    }

    pub(crate) unsafe fn as_obj<T>(&self) -> *mut T {
        (unsafe { self.r#as.obj } as *mut T)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ValueType {
    Bool,
    Nil,
    Number,
    Obj,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union ValueAs {
    pub(crate) boolean: bool,
    pub(crate) number: f64,
    pub(crate) obj: *mut Obj,
}
