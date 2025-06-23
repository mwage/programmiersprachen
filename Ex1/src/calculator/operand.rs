use std::ops::*;

#[derive(Clone)]
pub enum Operand {
    Integer(i64),
    Float(f64),
    String(String),
}

impl ToString for Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Integer(i) => i.to_string(),
            Operand::Float(f) => f.to_string(), // TODO: Remove redundant decimals
            Operand::String(s) => s.clone()
        }
    }
}

impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {    
        unimplemented!();    
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => false,
        }
        // TODO
    }
}

impl Eq for Operand {}

impl PartialOrd for Operand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        unimplemented!()
        // TODO
    }
}

impl Ord for Operand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unimplemented!()
        // TODO
    }
}

impl Add for Operand {
    type Output = Self;
    
    fn add(self, rhs: Self) -> Self {
        unimplemented!();
        todo!()
    }
}


impl Sub for Operand {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}

impl Mul for Operand {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}

impl Div for Operand {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}

impl Rem for Operand {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}

impl BitAnd for Operand {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}

impl BitOr for Operand {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        unimplemented!();
    }
}