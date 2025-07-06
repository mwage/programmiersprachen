use std::collections::{HashMap, VecDeque};
use super::{Operand, EPSILON};


pub struct Calculator {
    commands: VecDeque<char>, // A stream of characters regarded as commands to be executed in sequential order.
    operation_mode: i8, // An integer used in controlling the interpretation of commands
    data: Vec<Operand>, // This is the stack holding integers, floating-point numbers and strings when evaluating expressions in post-fix notation.
    registers: HashMap<char, Operand>, // A set of 52 read-only registers named by letters A to Z and a to z, each holding a single integer, floating-point number or string   // NOTE: These are constants (code that exists when switching on the calculator)
    input: String, // Input stream: A stream of characters typed in using the keyboard.    // NOTE: This is unparsed input (FIFO queue or just string?)
    output: String// Output stream: A stream of characters displayed on the screen.
}

impl Calculator {
    pub fn new() -> Self {
        let mut registers = HashMap::new();
        for c in 'A'..='Z' {
            registers.insert(c, Operand::String(String::new()));    // TODO: Empty string with or without ()?
        }
        for c in 'a'..='z' {
            registers.insert(c, Operand::String(String::new()));    // TODO: Empty string with or without ()?
        }
        Calculator { 
            commands: VecDeque::new(),   // TODO: init with register a
            operation_mode: 0,
            data: Vec::new(),
            registers,
            input: String::new(),
            output: String::new()
        }
    }

    pub fn turn_on(&mut self) {
        self.data = Vec::new();
        self.operation_mode = 0;
        // TODO: Initialize some register code
        self.commands.extend(self.registers.get(&'a').unwrap().to_string().chars());

        self.execute_commands();
    }

    /// The first character in the command stream, we call it command character,
    /// gets executed. On execution this character is removed from
    /// the command stream and the next character becomes executable.
    /// The kind of character and the operation mode together determine
    /// the operation to be executed.
    fn execute_commands(&mut self) {
        while let Some(next_command) = self.commands.pop_front() {
            match self.operation_mode {
                0 => self.execution(next_command),
                -1 => self.integer_construction(next_command),
                m if m < -1 => self.decimal_place_construction(next_command),
                _ => self.string_construction(next_command)
            }
        }
    }

    fn integer_construction(&mut self, next_command: char) {
        assert!(!self.data.is_empty()); // Data stack cannot be empty in integer construction mode
        let current = if let Operand::Integer(i) = self.data.last().unwrap() {
            *i
        } else {
            panic!("Current data must be integer in integer construction mode")
        };

        match next_command {
            '.' => {    // Transform int to float
                *self.data.last_mut().unwrap() = Operand::Float(current as f64);
            },      
            x if x.is_digit(10) => {    // Append digit to current int
                *self.data.last_mut().unwrap() = Operand::Integer(10 * current + i64::from(x.to_digit(10).unwrap()));
            },    
            _ => {      // Execute command in execution mode 
                self.operation_mode = 0;
                self.execution(next_command);
            }
        }
    }

    fn decimal_place_construction(&mut self, next_command: char) {
        assert!(!self.data.is_empty()); // Data stack cannot be empty in decimal place construction mode
        let current = if let Operand::Float(f) = self.data.last().unwrap() {
            *f
        } else {
            panic!("Current data must be float in decimal place construction mode")
        };

        match next_command {
            '.' => {    // Start a new float
                self.data.push(Operand::Float(0.0));
                self.operation_mode = -2;
            },      
            x if x.is_digit(10) => {    // Append digit to current int
                *self.data.last_mut().unwrap() = Operand::Float(current + 10f64.powf((self.operation_mode + 1) as f64) * f64::from(x.to_digit(10).unwrap()));
                self.operation_mode -= 1;
            },    
            _ => {      // Execute command in execution mode 
                self.operation_mode = 0;
                self.execution(next_command);
            }
        }
    }

    fn string_construction(&mut self, next_command: char) {
        assert!(!self.data.is_empty()); // Data stack cannot be empty in string construction mode
        assert!(matches!(self.data.last().unwrap(), Operand::String(_)));   // Current data must be string in string construction mode

        match next_command {
            '(' => {    // Increase string nesting
                if let Operand::String(x) = self.data.last_mut().unwrap() {
                    x.push(next_command);
                }
                self.operation_mode += 1;
            },
            ')' => {    // Reduce string nesting
                if self.operation_mode > 1 {
                    if let Operand::String(x) = self.data.last_mut().unwrap() {
                        x.push(next_command);
                    }
                }
                self.operation_mode -= 1;
            },
            _ => {  // Add to current string
                if let Operand::String(x) = self.data.last_mut().unwrap() {
                    x.push(next_command);
                }
            }
        }
    }

    fn execution(&mut self, next_command: char) {
        match next_command {
            '.' => {    // Go to decimal place construction mode
                self.data.push(Operand::Float(0.0));
                self.operation_mode = -2;
            },      
            x if x.is_digit(10) => {    // Go to integer construction mode
                self.data.push(Operand::Float(0.0));
                self.operation_mode = -1;
            },
            '(' => {    // Go to string construction mode
                self.data.push(Operand::String(String::new()));
                self.operation_mode = 1;
            },
            c if ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) => {    // Go to string construction mode
                self.data.push(self.registers.get(&c).unwrap().clone());
            },
            '=' | '<' | '>' => {    // Comparisons
                assert!(self.data.len() >= 2); // TODO: Replace with proper error handling
                let y = self.data.pop().unwrap();
                let x = self.data.pop().unwrap();
                let result = match next_command {
                    '=' => x == y,
                    '<' => x < y,
                    _ => x > y
                };
                self.data.push(Operand::Integer(if result { 1 } else { 0 }));
            },
            '+' | '-' | '*' | '/' | '%' => {    // Arithmetics
                assert!(self.data.len() >= 2); // TODO: Replace with proper error handling
                let y = self.data.pop().unwrap();
                let x = self.data.pop().unwrap();
                let result = match next_command {
                    '+' => x + y,
                    '-' => x - y,
                    '*' => x * y,
                    '/' => x / y,
                    _ => x % y,
                };
                self.data.push(result);
            },
            '|' | '&' => {  // Boolean logic
                assert!(self.data.len() >= 2); // TODO: Replace with proper error handling
                let y = self.data.pop().unwrap();
                let x = self.data.pop().unwrap();
                let result = if next_command == '|' {
                    x | y
                } else {
                    x & y
                };
                self.data.push(result);
            },
            '_' => {    // Null check
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(i) => i == 0,
                    Operand::Float(f) => f.abs() <= EPSILON,
                    Operand::String(s) => s.is_empty()
                };
                self.data.push(Operand::Integer(if result { 1 } else { 0 }));
            },
            '~' => {    // Negation
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(i) => Operand::Integer(-i),
                    Operand::Float(f) => Operand::Float(-f),
                    Operand::String(_) => Operand::String(String::new())
                };
                self.data.push(result);
            },
            '?' => {    // Integer Conversion
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(_) => Operand::String(String::new()),
                    Operand::Float(f) => Operand::Integer(f as i64),
                    Operand::String(_) => Operand::String(String::new())
                };
                self.data.push(result);
            },
            '!' => {    // Copy
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                match self.data.last().unwrap() {
                    Operand::Integer(i) => {
                        let len = self.data.len() - 1;  // len-1 since pop happens after (implicitly as a replace)
                        let i = *i;
                        if i as usize > len || i <= 0 {   // Invalid indices, 
                            return;
                        }

                        let new = self.data[len - i as usize].clone();
                        *self.data.last_mut().unwrap() = new;
                    }
                    _ => return,
                }
            },
            '$' => {    // Delete
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                match self.data.pop().unwrap() {
                    Operand::Integer(i) => {
                        let len = self.data.len();
                        if i as usize > len || i <= 0 {   // Invalid indices
                            return;
                        }
                        
                        self.data.remove(len - i as usize);
                    }
                    _ => return,
                }
            },
            '@' => {    // Apply immediately
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                if !matches!(self.data.last().unwrap(), Operand::String(_)) {
                    return;
                }
                
                if let Operand::String(s) = self.data.pop().unwrap() {
                    for c in s.chars().rev() {
                        self.commands.push_front(c);    // TODO: Check if this is correct, do I need to consider ()? reverse correct?
                    }
                }
            },
            '\\' => {    // Apply later
                assert!(!self.data.is_empty()); // TODO: Replace with proper error handling
                if !matches!(self.data.last().unwrap(), Operand::String(_)) {
                    return;
                }
                
                if let Operand::String(s) = self.data.pop().unwrap() {
                    for c in s.chars() {
                        self.commands.push_back(c);    // TODO: Check if this is correct, do I need to consider ()? reverse correct?
                    }
                }
            },
            '#' => {
                self.data.push(Operand::Integer(self.commands.len() as i64));
            },
            '\'' => {   // Read input
                unimplemented!()
            },
            '"' => {   // Write output
                unimplemented!()
            },
            _ => return
        }
    }

}

#[cfg(test)]
mod test {
    use super::*;

    // Basic functionality tests

    fn new_calculator() -> Calculator {
        let mut calculator = Calculator::new();
        calculator.turn_on();
        
        calculator
    }

    fn evaluate(calculator: &mut Calculator, input: &str) -> String {
        calculator.add_input(input);
        calculator.execute_commands();

        calculator.get_output().to_string()
    }

    #[test]
    fn test_addition() {
        let mut calculator = new_calculator();
        calculator.turn_on();
        assert_eq!("17.4", evaluate(&mut calculator, "5.1 12.3+"));
    }

    #[test]
    fn test_basic_operations() {
        let mut calculator = new_calculator();
        assert_eq!("1".to_string(), evaluate(&mut calculator, "15 2 3 4+*-"));
    }

    #[test]
    fn test_string_eval() {
        let mut calculator = new_calculator();
        assert_eq!("10".to_string(), evaluate(&mut calculator, "4 3(2*)@+"));
    }

    #[test]
    fn test_div() {
        let mut calculator = new_calculator();
        assert_eq!("2".to_string(), evaluate(&mut calculator, "4 2/"));
    }

    #[test]
    fn test_mod() {
        let mut calculator = new_calculator();
        assert_eq!("1".to_string(), evaluate(&mut calculator, "4 3%"));
    }

    #[test]
    fn test_conditionals() {
        let mut calculator = new_calculator();
        assert_eq!("8".to_string(), evaluate(&mut calculator, "(4!4$_1+$@)"));
    }
}