use std::collections::{HashMap, VecDeque};
use super::{Operand, EPSILON};
use std::io::stdin;


pub struct Calculator {
    commands: VecDeque<char>, // A stream of characters regarded as commands to be executed in sequential order.
    operation_mode: i8, // An integer used in controlling the interpretation of commands
    data: Vec<Operand>, // This is the stack holding integers, floating-point numbers and strings when evaluating expressions in post-fix notation.
    registers: HashMap<char, Operand>, // A set of 52 read-only registers named by letters A to Z and a to z, each holding a single integer, floating-point number or string   // NOTE: These are constants (code that exists when switching on the calculator)
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
        eprintln!("Enter integer construction");
        assert!(!self.data.is_empty()); // Data stack cannot be empty in integer construction mode
        eprintln!("Data: {:?}", self.data.last().unwrap());
        let current = if let Operand::Integer(i) = self.data.last().unwrap() {
            *i
        } else {
            panic!("Current data must be integer in integer construction mode")
        };
        eprintln!("Current: {}", current);

        match next_command {
            '.' => {    // Transform int to float
                *self.data.last_mut().unwrap() = Operand::Float(current as f64);
                self.operation_mode = -2;
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
        eprintln!("Enter decimal construction");
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
        eprintln!("Enter string construction");
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
        eprintln!("Enter execution mode");
        match next_command {
            '.' => {    // Go to decimal place construction mode
                self.data.push(Operand::Float(0.0));
                self.operation_mode = -2;
            },      
            x if x.is_digit(10) => {    // Go to integer construction mode
                self.data.push(Operand::Integer(x.to_digit(10).unwrap() as i64));
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
                // TODO: Ignore ASCII
                let mut input = String::new();  // Input stream
                stdin().read_line(&mut input).expect("Failed to read line.");  // TODO: Replace with proper error handling
                input = String::from(input.trim());
                if let Ok(i) = input.parse::<i64>() {  // Try parse to int
                    self.data.push(Operand::Integer(i));
                } else if let Ok(f) = input.parse::<f64>() {  // Try parse to float
                    self.data.push(Operand::Float(f));
                } else {
                    // Otherwise push string
                    self.data.push(Operand::String(input.clone()))
                }
            },
            '"' => {   // Write output
                if let Some(x) = self.data.pop() {
                    print!("{}", x.to_string());    // Output
                }
            },
            _ => return
        }
    }

}

/// Tests the implementation of all predefined operations for correctness
#[cfg(test)]
mod functinoality_tests {
    use super::*;

    // Helper function for testing the internal workings of the calculator
    fn execute_input(input: &str) -> String {
        let mut calculator = Calculator::new();
        calculator.commands.extend(input.chars());
        calculator.execute_commands();
        calculator.data.pop().unwrap().to_string()
    }

    #[test]
    fn test_construction() {
        // Integers
        assert_eq!("1", execute_input("1"));
        assert_eq!("42", execute_input("42"));
        assert_eq!("2", execute_input("1 2"));
        assert_eq!("-1", execute_input("1~"));  // Negative numbers
        // Floats
        assert_eq!("0.1", execute_input("0.1"));
        assert_eq!("10.1", execute_input("10.1"));
        assert_eq!("0.0", execute_input("0.0"));
        assert_eq!("0.1", execute_input("0.10"));   // Avoid unnecessary 0s
        assert_eq!("0.103", execute_input("0.103"));
        assert_eq!("0.0", execute_input("0.0000000000000001")); // Remove redundancy smaller than our precision
        assert_eq!("-1.3", execute_input("1.3~"));  // Negative numbers
        // Strings
        assert_eq!("Hello World!", execute_input("(Hello World!)"));
        assert_eq!("Un(Bal(anced()", execute_input("(Un(Bal(anced()"));
        assert_eq!("Three!", execute_input("3 (Three!)"));  // Int + String
    }

    #[test]
    fn test_equality() {
        // Ints
        assert_eq!("1", execute_input("3 3="));
        assert_eq!("0", execute_input("3 2="));
        // Floats
        assert_eq!("1", execute_input("3.0 3.0=")); 
        assert_eq!("0", execute_input("3.0 3.00005=")); 
        assert_eq!("1", execute_input("3.0 3.000000000000000000001="));
        // Float + Int
        assert_eq!("1", execute_input("3 3.0="));
        assert_eq!("0", execute_input("3 3.000002="));
        // Strings
        assert_eq!("1", execute_input("(Hello) (Hello)="));
        assert_eq!("0", execute_input("(Hello) (Hello World!)="));
        // Strings + other
        assert_eq!("0", execute_input("(1) 1="));
        assert_eq!("0", execute_input("(0.0) 0.0="));
    }
    
    
    #[test]
    fn test_greater() {
        // Ints
        assert_eq!("1", execute_input("3 2>"));
        assert_eq!("0", execute_input("3 4>"));
        // Floats
        assert_eq!("1", execute_input("3.1 3.0>")); 
        assert_eq!("0", execute_input("3.0 3.00005>")); 
        assert_eq!("0", execute_input("3.0 3.000000000000000000001>"));
        // Float + Int
        assert_eq!("1", execute_input("3 2.0>"));
        assert_eq!("0", execute_input("3 3.000002>"));
        // Strings
        assert_eq!("0", execute_input("(Hello) (Hello)<"));
        assert_eq!("1", execute_input("(Hello) (Hello World!)<"));
        assert_eq!("1", execute_input("(Hello) (World)<"));
        // Strings + other
        assert_eq!("1", execute_input("(1) 1>"));
        assert_eq!("1", execute_input("(0.0) 0.0>"));
    }
    
    #[test]
    fn test_addition() {
        assert_eq!("5", execute_input("3 2+")); // Ints
        assert_eq!("7.1", execute_input("3.3 3.8+"));   // Floats
        assert_eq!("6.05", execute_input("3 3.05+"));     // Int + Float
        assert_eq!("6.05", execute_input("3.05 3+"));     // Int + Float
        assert_eq!("Hello World!", execute_input("(Hello ) (World!)+")); // Strings
        assert_eq!("Three: 3", execute_input("(Three: ) 3+"));  // String + Int
        assert_eq!("3: Three", execute_input("3 (: Three)+"));  // String + Int
        assert_eq!("Threes: 3.3", execute_input("(Threes: ) 3.3+"));  // String + Float
        assert_eq!("Three: 3.0", execute_input("(Three: ) 3.000000000000000000001+"));  // String + Float
        assert_eq!("3.3: Threes", execute_input("3.3 (: Threes)+"));  // String + Float
    }

    #[test]
    fn test_subtraction() {
        assert_eq!("1", execute_input("3 2-")); // Ints
        assert_eq!("-0.5", execute_input("3.3 3.8-"));   // Floats
        assert_eq!("-0.05", execute_input("3 3.05-"));     // Int + Float
        assert_eq!("0.05", execute_input("3.05 3-"));     // Int + Float
        assert_eq!("", execute_input("(Hello ) (World!)-")); // Strings
        assert_eq!("", execute_input("(Hello ) (llo)-")); // Strings
        assert_eq!("Hel", execute_input("(Hello) 2-"));  // String + Int
        assert_eq!("lo", execute_input("3 (Hello)-"));  // String + Int
        assert_eq!("", execute_input("(Hello) 3.0-"));  // String + Float
        assert_eq!("", execute_input("3.3 (Hello)-"));  // String + Float
    }

    #[test]
    fn test_multiplication() {
        assert_eq!("42", execute_input("7 6*")); // Ints
        assert_eq!("42.0", execute_input("7.5 5.6*"));   // Floats
        assert_eq!("15.9", execute_input("3 5.3*"));     // Int + Float
        assert_eq!("15.9", execute_input("5.3 3*"));     // Int + Float
        assert_eq!("", execute_input("(Hello ) (World!)*")); // Strings
        assert_eq!("Hello World!", execute_input("(Hello World) 33*"));  // String + Int
        assert_eq!("Hi", execute_input("72 (i)*"));  // String + Int
        assert_eq!("", execute_input("(Threes: ) 3.3*"));  // String + Float
        assert_eq!("", execute_input("3.3 (: Threes)*"));  // String + Float
    }

    #[test]
    fn test_division() {
        assert_eq!("6", execute_input("42 7/")); // Ints
        assert_eq!("0", execute_input("2 4/")); // Ints
        assert_eq!("7.5", execute_input("42.0 5.6/"));   // Floats
        assert_eq!("2.0", execute_input("3 1.5/"));     // Int + Float
        assert_eq!("1.5", execute_input("4.5 3/"));     // Int + Float
        assert_eq!("-1", execute_input("(Hello) (World!)/")); // Strings
        assert_eq!("6", execute_input("(Hello World) (World)/"));  // Strings
        assert_eq!("", execute_input("(Hello) 1/"));  // String + Int
        assert_eq!("", execute_input("1 (Hello)/"));  // String + Int
        assert_eq!("", execute_input("(Threes: ) 3.3/"));  // String + Float
        assert_eq!("", execute_input("3.3 (: Threes)/"));  // String + Float
    }

    #[test]
    fn test_mod() {
        assert_eq!("0", execute_input("42 7%")); // Ints
        assert_eq!("2", execute_input("42 8%")); // Ints
        assert_eq!("", execute_input("42.0 5.6%"));   // Floats
        assert_eq!("", execute_input("3 1.5%"));     // Int + Float
        assert_eq!("", execute_input("4.5 3%"));     // Int + Float
        assert_eq!("", execute_input("(Hello) (Hello)%")); // Strings
        assert_eq!("", execute_input("(Hello World) (World)%"));  // Strings
        assert_eq!("101", execute_input("(Hello) 1%"));  // String + Int    (returns ASCII of e)
        assert_eq!("", execute_input("1 (Hello)%"));  // String + Int
        assert_eq!("", execute_input("(Threes: ) 3.3%"));  // String + Float
        assert_eq!("", execute_input("3.3 (: Threes)%"));  // String + Float
    }
}