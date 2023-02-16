use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    IntTy,   // 32 bit integer type
    FloatTy, // single precision float
    StringTy,
    BoolTy,
    FunctionTy(Vec<Rc<Type>>, Box<Type>),
    AnyTy,

    // The type is unknown/has not been inferred yet
    UnknownTy,
}

impl Type {
    /// Return whether the type is numeric or not
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::IntTy | Self::FloatTy => true,
            _ => false,
        }
    }

    /// Return whether two types are compatible with each other
    pub fn is_compatible(&self, other: Type) -> bool {
        match (self, other) {
            (_, Self::AnyTy) => true,
            (self_, other) => *self_ == other,
        }
    }
}
