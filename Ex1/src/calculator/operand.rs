use std::{cmp::Ordering, ops::*};
use super::EPSILON;

/// The available datatypes of the calculator (Integers, Floats and Strings)
#[derive(Clone, Debug)]
pub enum Operand {
    Integer(i64),
    Float(f64),
    String(String),
}

impl ToString for Operand {
    /// Returns the string representation of the different datatypes of the calculator
    fn to_string(&self) -> String {
        match self {
            Operand::Integer(i) => i.to_string(),
            Operand::Float(f) => {
                if f.abs() < EPSILON { return String::from("0.0") } // Catches -0.0

                let precision: f64 = 1.0/EPSILON;
                let rounded = (f * precision).round() / precision;
                if rounded.fract().abs() < EPSILON {
                    return format!("{:.1}", rounded);
                }

                rounded.to_string()
            }
            Operand::String(s) => s.clone()
        }
    }
}

impl PartialEq for Operand {
    /// Returns whether two operates are equal to each other according to specification
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

/// Automatically derives inequality
impl Eq for Operand {}

/// Implements the ordering for each of the datatypes according to the specification
impl PartialOrd for Operand {
    /// Returns if the other value is Less, Equal or Greater than the value
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

/// Ordering is defined for all datatypes and all values => we have a total ordering and can just rely on the PartialOrd implementation
impl Ord for Operand {
    /// Returns if the other value is Less, Equal or Greater than the value
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
       self.partial_cmp(other).unwrap()
    }
}

/// Implements addition for each of the datatypes according to the specification
impl Add for Operand {
    type Output = Self;
        
    /// Returns the sum of the two values
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => Self::Integer(l0 + r0),
            (Self::Float(l0), Self::Float(r0)) => Self::Float(l0 + r0),
            (Self::Float(l0), Self::Integer(r0)) => Self::Float(l0 + r0 as f64),
            (Self::Integer(l0), Self::Float(r0)) => Self::Float(l0 as f64 + r0),
            (Self::String(l0), Self::String(r0)) => Self::String(l0 + &r0),
            (Self::String(l0), x) => Self::String(l0 + &x.to_string()),
            (x, Self::String(r0)) => Self::String(x.to_string() + &r0)
        }
    }
}

/// Implements subtraction for each of the datatypes according to the specification
impl Sub for Operand {
    type Output = Self;

    /// Returns self minus the the second value (rhs)
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => Self::Integer(l0 - r0),
            (Self::Float(l0), Self::Float(r0)) => Self::Float(l0 - r0),
            (Self::Float(l0), Self::Integer(r0)) => Self::Float(l0 - r0 as f64),
            (Self::Integer(l0), Self::Float(r0)) => Self::Float(l0 as f64 - r0),
            (Self::String(l0), Self::Integer(r0)) => {  // Remove from right
                if r0 < 0 || r0 >= l0.len() as i64 {
                    return Self::String(String::new())  // Int negative or removing all chars (and more)
                }

                Self::String(l0.chars().take(l0.len() - r0 as usize).collect::<String>()) // Remove last l0 chars
            },
            (Self::Integer(l0), Self::String(r0)) => {  // Remove from left
                if l0 < 0 || l0 >= r0.len() as i64 {
                    return Self::String(String::new())  // Int negative or removing all chars (and more)
                }

                Self::String(r0.chars().skip(l0 as usize).collect::<String>())  // Remove first l0 chars
            },
            _ => Self::String(String::new())
        }
    }
}

/// Implements multiplication for each of the datatypes according to the specification
impl Mul for Operand {
    type Output = Self;

    /// Returns the product of the two values
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => Self::Integer(l0 * r0),
            (Self::Float(l0), Self::Float(r0)) => Self::Float(l0 * r0),
            (Self::Float(l0), Self::Integer(r0)) => Self::Float(l0 * r0 as f64),
            (Self::Integer(l0), Self::Float(r0)) => Self::Float(l0 as f64 * r0),
            (Self::String(mut l0), Self::Integer(r0)) => {  // Add ASCII to right
                if r0 < 0 || r0 > 128 as i64 {
                    return Self::String(String::new())  // Int not ASCII
                }
                l0.push(r0 as u8 as char);
                Self::String(l0) // Remove last l0 chars
            },
            (Self::Integer(l0), Self::String(mut r0)) => {  // Add ASCII to left
                if l0 < 0 || l0 > 128 as i64 {
                    return Self::String(String::new())  // Int not ASCII
                }

                r0.insert(0, l0 as u8 as char);
                Self::String(r0)  // Remove first l0 chars
            },
            _ => Self::String(String::new())
        }
    }
}

/// Implements division for each of the datatypes according to the specification
impl Div for Operand {
    type Output = Self;

    /// Returns self divided by the other value (rhs)
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => {
                if r0 == 0 { Self::String(String::new()) } else { Self::Integer(l0 / r0) }
            },
            (Self::Float(l0), Self::Float(r0)) => {
                if r0.abs() < EPSILON { Self::String(String::new()) } else { Self::Float(l0 / r0) }
            },
            (Self::Float(l0), Self::Integer(r0)) => {
                if r0 == 0 { Self::String(String::new()) } else { Self::Float(l0 / r0 as f64) } 
            },
            (Self::Integer(l0), Self::Float(r0)) => {
                if r0.abs() < EPSILON { Self::String(String::new()) } else { Self::Float(l0 as f64 / r0) }
            },
            (Self::String(l0), Self::String(r0)) => {  // Returns first position where the second string occurs in the first string
                match l0.find(&r0) {
                    Some(i) => Self::Integer(i as i64),
                    None => Self::Integer(-1)
                }
            },
            _ => Self::String(String::new())
        }
    }
}

/// Implements the remainder operation for each of the datatypes according to the specification
impl Rem for Operand {
    type Output = Self;

    /// Returns the remainder of self divided by the other value (rhs)
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => Self::Integer(l0 % r0),
            (Self::String(l0), Self::Integer(r0)) => {  // Get ASCII at int position
                match l0.chars().nth(r0 as usize - 1) {   // One-index for consistency instead of Rust default 0-index
                    Some(c) => Self::Integer(c as u8 as i64),   // Index found
                    None => Self::String(String::new()) // Index out of range
                }
            },
            _ => Self::String(String::new())
        }
    }
}

/// Implements bitwise And for each of the datatypes according to the specification
impl BitAnd for Operand {
    type Output = Self;

    /// Returns the bitwise and between self and the other value (rhs)
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => {
                Self::Integer(if l0 == 0 || r0 == 0 { 0 } else { 1 })
            },
            _ => Self::String(String::new())
        }
    }
}

/// Implements bitwise Or for each of the datatypes according to the specification
impl BitOr for Operand {
    type Output = Self;

    /// Returns the bitwise and between self and the other value (rhs)
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l0), Self::Integer(r0)) => {
                Self::Integer(if l0 == 0 && r0 == 0 { 0 } else { 1 })
            },
            _ => Self::String(String::new())
        }
    }
}