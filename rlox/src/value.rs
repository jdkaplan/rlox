use std::fmt;

use crate::{object::print_object, Obj};

#[repr(C)]
pub struct Value {
    r#type: ValueType,
    r#as: ValueAs,
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
            ValueType::TObj => write!(f, "<object Object>"), // TODO
        }
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

/// cbindgen:rename-all=ScreamingSnakeCase
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ValueType {
    TBool,
    TNil,
    TNumber,
    TObj,
}

#[repr(C)]
pub union ValueAs {
    boolean: bool,
    number: f64,
    obj: *mut Obj,
}
