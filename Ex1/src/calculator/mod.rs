mod calculator;
mod operand;

pub const EPSILON: f64 = 1e-10;  // Floating point precision (f64 has precision of ~15-17 decimal digits, so this is easily within that threshold) 

pub use calculator::Calculator;
use operand::Operand;