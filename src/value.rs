use std::fmt;

use crate::object::{print_object, Obj, ObjType};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Value {
    pub(crate) r#type: ValueType,
    pub(crate) r#as: ValueAs,
}

impl Value {
    pub(crate) fn bool(boolean: bool) -> Self {
        Self {
            r#type: ValueType::TBool,
            r#as: ValueAs { boolean },
        }
    }

    pub(crate) fn nil() -> Self {
        Self {
            r#type: ValueType::TNil,
            r#as: ValueAs { number: 0.0 },
        }
    }

    pub(crate) fn number(number: f64) -> Self {
        Self {
            r#type: ValueType::TNumber,
            r#as: ValueAs { number },
        }
    }

    pub(crate) fn obj(obj: *mut Obj) -> Self {
        Self {
            r#type: ValueType::TObj,
            r#as: ValueAs { obj },
        }
    }
}

impl Value {
    pub(crate) fn is_bool(&self) -> bool {
        self.r#type == ValueType::TBool
    }

    pub(crate) fn is_nil(&self) -> bool {
        self.r#type == ValueType::TNil
    }

    pub(crate) fn is_number(&self) -> bool {
        self.r#type == ValueType::TNumber
    }

    pub(crate) fn is_obj(&self) -> bool {
        self.r#type == ValueType::TObj
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
            ValueType::TBool => {
                if unsafe { self.r#as.boolean } {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            ValueType::TNil => write!(f, "nil"),
            ValueType::TNumber => write!(f, "{}", unsafe { self.r#as.number }),
            ValueType::TObj => write!(f, "{}", unsafe { self.r#as.obj.as_ref().unwrap() }),
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
                    ValueType::TBool => unsafe { &self.r#as.boolean },
                    ValueType::TNil => &"nil",
                    ValueType::TNumber => unsafe { &self.r#as.number },
                    ValueType::TObj => unsafe { &self.r#as.obj },
                },
            )
            .finish()
    }
}

impl Value {
    pub fn print(&self) {
        match self.r#type {
            ValueType::TBool => {
                if unsafe { self.r#as.boolean } {
                    print!("true")
                } else {
                    print!("false")
                }
            }
            ValueType::TNil => print!("nil"),
            ValueType::TNumber => print!("{}", unsafe { self.r#as.number }),
            ValueType::TObj => print_object(unsafe { self.r#as.obj.as_mut().unwrap() }),
        }
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self.r#type, other.r#type) {
            (ValueType::TBool, ValueType::TBool) => unsafe {
                self.r#as.boolean == other.r#as.boolean
            },
            (ValueType::TBool, _) => false,

            (ValueType::TNil, ValueType::TNil) => true,
            (ValueType::TNil, _) => false,

            (ValueType::TNumber, ValueType::TNumber) => unsafe {
                self.r#as.number == other.r#as.number
            },
            (ValueType::TNumber, _) => false,

            // The VM interns all strings, so pointer equality works for every object.
            (ValueType::TObj, ValueType::TObj) => unsafe { self.r#as.obj == other.r#as.obj },
            (ValueType::TObj, _) => false,
        }
    }
}

impl Value {
    pub(crate) fn is_obj_type(&self, ty: ObjType) -> bool {
        self.r#type == ValueType::TObj && unsafe { self.r#as.obj.as_ref().unwrap() }.r#type == ty
    }

    pub(crate) unsafe fn as_obj<T>(&self) -> *mut T {
        (unsafe { self.r#as.obj } as *mut T)
    }
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ValueType {
    TBool,
    TNil,
    TNumber,
    TObj,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union ValueAs {
    pub(crate) boolean: bool,
    pub(crate) number: f64,
    pub(crate) obj: *mut Obj,
}
