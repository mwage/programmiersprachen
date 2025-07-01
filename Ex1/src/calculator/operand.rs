use std::{cmp::Ordering, ops::*};
use super::EPSILON;

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
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => {
                let al0 = f64::abs(*l0);
                let ar0 = f64::abs(*r0);
                if al0 < 1.0 && ar0 < 1.0 {
                    f64::abs(l0 - r0) < EPSILON
                } else {
                    if al0 < ar0 {
                        f64::abs(l0 - r0) < EPSILON * ar0
                    } else {
                        f64::abs(l0 - r0) < EPSILON * al0
                    }
                }                
            },
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Float(r0)) => {
                Self::Float(*l0 as f64) == Self::Float(*r0)
            },
            (Self::Float(l0), Self::Integer(r0)) => {
                Self::Float(*l0) == Self::Float(*r0 as f64)
            },
            _ => false,
        }
    }
}

impl Eq for Operand {}

impl PartialOrd for Operand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => Some(l0.cmp(r0)),
            (Self::Float(l0), Self::Float(r0)) => {
                if Self::Float(*l0) == Self::Float(*r0) {   // Equality check respecting EPSILON (as implemented above)
                    return Some(Ordering::Equal)
                }
                if l0 < r0 {
                    return Some(Ordering::Less)
                }
                Some(Ordering::Greater)
            },
            (Self::String(l0), Self::String(r0)) => Some(l0.cmp(r0)),
            (Self::Integer(l0), Self::Float(r0)) => {
                Some(Self::Float(*l0 as f64).cmp(&Self::Float(*r0)))
            },
            (Self::Float(l0), Self::Integer(r0)) => {
                Some(Self::Float(*l0).cmp(&Self::Float(*r0 as f64)))
            },
            (Self::String(_), _) => Some(Ordering::Greater),
            (_, Self::String(_)) => Some(Ordering::Less)
        }
    }
}

impl Ord for Operand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
       self.partial_cmp(other).unwrap()
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