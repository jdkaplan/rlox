use std::fmt;
use std::ptr::NonNull;

use crate::object::{Obj, ObjType};

#[derive(Debug, Copy, Clone, strum::EnumDiscriminants)]
#[strum_discriminants(name(ValueType))]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(NonNull<Obj>),
}

impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

impl Value {
    pub(crate) fn bool(boolean: bool) -> Self {
        Value::Bool(boolean)
    }

    pub(crate) fn nil() -> Self {
        Value::Nil
    }

    pub(crate) fn number(number: f64) -> Self {
        Value::Number(number)
    }

    pub(crate) fn obj(obj: NonNull<Obj>) -> Self {
        Value::Obj(obj)
    }
}

#[allow(unused)]
impl Value {
    pub(crate) fn is(&self, ty: ValueType) -> bool {
        match self {
            Value::Bool(_) => ty == ValueType::Bool,
            Value::Nil => ty == ValueType::Nil,
            Value::Number(_) => ty == ValueType::Number,
            Value::Obj(_) => ty == ValueType::Obj,
        }
    }

    pub(crate) fn is_bool(&self) -> bool {
        self.is(ValueType::Bool)
    }

    pub(crate) fn is_nil(&self) -> bool {
        self.is(ValueType::Nil)
    }

    pub(crate) fn is_number(&self) -> bool {
        self.is(ValueType::Number)
    }

    pub(crate) fn is_obj(&self) -> bool {
        self.is(ValueType::Obj)
    }

    pub(crate) fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}

#[allow(unused)]
impl Value {
    pub(crate) fn try_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub(crate) fn try_number(&self) -> Option<f64> {
        match self {
            Value::Number(f) => Some(*f),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Value::Nil => write!(f, "nil"),
            Value::Number(num) => write!(f, "{}", num),
            Value::Obj(obj) => write!(f, "{}", unsafe { obj.as_ref() }),
        }
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Bool(_), _) => false,

            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,

            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Number(_), _) => false,

            // The VM interns all strings, so pointer identity works for every object.
            (Value::Obj(a), Value::Obj(b)) => a == b,
            (Value::Obj(_), _) => false,
        }
    }
}

impl Value {
    pub(crate) fn is_obj_type(&self, ty: ObjType) -> bool {
        match self {
            Value::Obj(obj) => unsafe { obj.as_ref() }.ty == ty,
            _ => false,
        }
    }

    pub(crate) fn try_obj<T>(&self) -> Option<NonNull<T>> {
        match self {
            Value::Obj(obj) => Some(obj.cast::<T>()),
            _ => None,
        }
    }

    pub(crate) unsafe fn as_obj<T>(&self) -> NonNull<T> {
        self.try_obj::<T>().unwrap()
    }
}
