use std::{collections::{HashMap, VecDeque}};
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
            registers.insert(c, Operand::String(String::new()));
        }
        for c in 'a'..='z' {
            registers.insert(c, Operand::String(String::new()));
        }

        // TODO: Add code to registers!
        registers.insert('a', Operand::String(String::from("(Welcome to our awesome calculator!\n)\"b@")));
        registers.insert('b', Operand::String(String::from("
(What do you want to calculate?\n)
\"\'@
(The result is: )\"\"(\n)\"
()b@")));
        Calculator { 
            commands: VecDeque::new(),
            operation_mode: 0,
            data: Vec::new(),
            registers,
        }
    }

    pub fn turn_on(&mut self) {
        self.data = Vec::new();
        self.operation_mode = 0;
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
            let res = match self.operation_mode {
                0 => self.execution(next_command),
                -1 => self.integer_construction(next_command),
                m if m < -1 => self.decimal_place_construction(next_command),
                _ => self.string_construction(next_command)
            };
            if res.is_err() {
                println!("Error: {}\nShutting down...", res.err().unwrap());
                return;
            }
        }
    }

    fn integer_construction(&mut self, next_command: char) -> Result<(), String> {
        if self.data.is_empty() {
            return Err(String::from("Data stack cannot be empty in integer construction mode!"));
        }
        let current = if let Operand::Integer(i) = self.data.last().unwrap() {
            *i
        } else {
            return Err(String::from("Current data must be integer in integer construction mode!"));
        };

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
                self.execution(next_command)?;
            }
        }

        Ok(())
    }

    fn decimal_place_construction(&mut self, next_command: char) -> Result<(), String> {
        if self.data.is_empty() {
            return Err(String::from("Data stack cannot be empty in decimal place construction mode!"));
        }
        let current = if let Operand::Float(f) = self.data.last().unwrap() {
            *f
        } else {
            return Err(String::from("Current data must be float in decimal place construction mode!"));
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
                self.execution(next_command)?;
            }
        }

        Ok(())
    }

    fn string_construction(&mut self, next_command: char) -> Result<(), String> {
        if self.data.is_empty() {
            return Err(String::from("Data stack cannot be empty in string construction mode!"));
        }
        if !matches!(self.data.last().unwrap(), Operand::String(_)) {
            return Err(String::from("Current data must be string in string construction mode!"));
        }

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

        Ok(())
    }

    fn execution(&mut self, next_command: char) -> Result<(), String> {
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
                if self.data.len() < 2 {
                    return Err(format!("Comparison {} requires at least 2 items on data stack (currently {})!", next_command, self.data.len()));
                }
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
                if self.data.len() < 2 {
                    return Err(format!("Arithmetic operation {} require at least 2 items on data stack (currently {})!", next_command, self.data.len()));
                }
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
                if self.data.len() < 2 {
                    return Err(format!("Logic operation {} require at least 2 items on data stack (currently {})!", next_command, self.data.len()));
                }
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
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for null check!"));
                }
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(i) => i == 0,
                    Operand::Float(f) => f.abs() <= EPSILON,
                    Operand::String(s) => s.is_empty()
                };
                self.data.push(Operand::Integer(if result { 1 } else { 0 }));
            },
            '~' => {    // Negation
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for negation!"));
                }
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(i) => Operand::Integer(-i),
                    Operand::Float(f) => Operand::Float(-f),
                    Operand::String(_) => Operand::String(String::new())
                };
                self.data.push(result);
            },
            '?' => {    // Integer Conversion
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for integer conversion!"));
                }
                let x = self.data.pop().unwrap();
                let result = match x {
                    Operand::Integer(_) => Operand::String(String::new()),
                    Operand::Float(f) => Operand::Integer(f as i64),
                    Operand::String(_) => Operand::String(String::new())
                };
                self.data.push(result);
            },
            '!' => {    // Copy (1-indexed)
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for copy operator!"));
                }
                match self.data.last().unwrap() {
                    Operand::Integer(i) => {
                        let len = self.data.len() - 1;  // len-1 since pop happens after (implicitly as a replace)
                        let i = *i;
                        if i as usize > len || i <= 0 {   // Invalid indices, 
                            return Ok(());
                        }

                        let new = self.data[len - i as usize].clone();
                        *self.data.last_mut().unwrap() = new;
                    }
                    _ => return Ok(()),
                }
            },
            '$' => {    // Delete
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for deletion operator!"));
                }
                match self.data.pop().unwrap() {
                    Operand::Integer(i) => {
                        let len = self.data.len();
                        if i as usize > len || i <= 0 {   // Invalid indices
                            return Ok(());
                        }
                        
                        self.data.remove(len - i as usize);
                    }
                    _ => return Ok(()),
                }
            },
            '@' => {    // Apply immediately
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for application operator @!"));
                }
                if !matches!(self.data.last().unwrap(), Operand::String(_)) {
                    return Ok(());  // Not applicable, nothing happens
                }
                
                if let Operand::String(s) = self.data.pop().unwrap() {
                    for c in s.chars().rev() {
                        self.commands.push_front(c);
                    }
                }
            },
            '\\' => {    // Apply later
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for late application operator \\!"));
                }
                if !matches!(self.data.last().unwrap(), Operand::String(_)) {
                    return Ok(());  // Not applicable, nothing happens
                }
                
                if let Operand::String(s) = self.data.pop().unwrap() {
                    for c in s.chars() {
                        self.commands.push_back(c);
                    }
                }
            },
            '#' => {
                self.data.push(Operand::Integer(self.data.len() as i64));
            },
            '\'' => {   // Read input
                let mut input = String::new();  // Input stream
                stdin().read_line(&mut input).expect("Failed to read line.");
                input = input.trim().chars().filter(|c| c.is_ascii()).collect::<String>();  // Trim + remove non-asciis
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
            _ => return Ok (())
        }

        Ok(())
    }

}

/// Tests the implementation of all predefined operations for correctness
#[cfg(test)]
mod functionality_tests {
    use super::*;

    // Helper function for testing the internal workings of the calculator
    fn execute_input(input: &str) -> String {
        let mut calculator = Calculator::new();
        calculator.commands.extend(input.chars());
        calculator.execute_commands();
        match calculator.data.pop() {
            Some(x) => x.to_string(),
            None => String::from("")
        }
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
    }

    #[test]
    fn test_negation() {
        assert_eq!("-1", execute_input("1~"));    // Ints
        assert_eq!("1", execute_input("1~~"));    // Ints
        assert_eq!("0", execute_input("0~"));    // Ints
        assert_eq!("-1.3", execute_input("1.3~"));    // Floats
        assert_eq!("0.0", execute_input("0.0~"));    // Floats
        assert_eq!("", execute_input("(Hello)~"));    // Strings
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
        assert_eq!("", execute_input("42 0/")); // Ints
        assert_eq!("0", execute_input("2 4/")); // Ints
        assert_eq!("7.5", execute_input("42.0 5.6/"));   // Floats
        assert_eq!("", execute_input("42.0 0.0/"));   // Floats
        assert_eq!("2.0", execute_input("3 1.5/"));     // Int + Float
        assert_eq!("", execute_input("3 0.0/"));     // Int + Float
        assert_eq!("1.5", execute_input("4.5 3/"));     // Int + Float
        assert_eq!("", execute_input("4.5 0/"));     // Int + Float
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
        assert_eq!("101", execute_input("(Hello) 2%"));  // String + Int    (returns ASCII of e)
        assert_eq!("", execute_input("1 (Hello)%"));  // String + Int
        assert_eq!("", execute_input("(Threes: ) 3.3%"));  // String + Float
        assert_eq!("", execute_input("3.3 (: Threes)%"));  // String + Float
    }

    #[test]
    fn test_nullcheck() {
        assert_eq!("1", execute_input("0_")); // Ints
        assert_eq!("0", execute_input("1_")); // Ints
        assert_eq!("0", execute_input("1~_")); // Ints
        assert_eq!("1", execute_input("0.0_"));   // Floats
        assert_eq!("1", execute_input("0.000000000000001_"));   // Floats
        assert_eq!("0", execute_input("0.1_"));   // Floats
        assert_eq!("1", execute_input("()_")); // Strings
        assert_eq!("0", execute_input("( )_")); // Strings
        assert_eq!("0", execute_input("(Hello)_")); // Strings
    }

    #[test]
    fn test_int_conversion() {
        assert_eq!("", execute_input("0?")); // Ints
        assert_eq!("", execute_input("1?")); // Ints
        assert_eq!("0", execute_input("0.5?"));   // Floats
        assert_eq!("-1", execute_input("1.1~?"));   // Floats
        assert_eq!("", execute_input("()?")); // Strings
        assert_eq!("", execute_input("(Hello)?")); // Strings
    }

    #[test]
    fn test_copy() {
        assert_eq!("a", execute_input("(a) 1!"));
        assert_eq!("3", execute_input("(a) 3 0.5 2!"));
        assert_eq!("0.5", execute_input("(a) 3 0.5 2! 2!"));
        assert_eq!("6", execute_input("(a) 3 0.5 6!"));
        assert_eq!("5.3", execute_input("(a) 3 0.5 5.3!"));
        assert_eq!("a", execute_input("(a) 3 0.5 (a)!"));
    }

    #[test]
    fn test_delete() {
        assert_eq!("", execute_input("(a) 1$"));
        assert_eq!("a", execute_input("(a) 3 1$"));
        assert_eq!("3", execute_input("(a) 3 2$"));
        assert_eq!("a", execute_input("(a) 3 0.5 2$ 2!"));
        assert_eq!("a", execute_input("(a) 3 0.5 1$ 1$"));
        assert_eq!("0.5", execute_input("(a) 3 0.5 6$"));
        assert_eq!("0.5", execute_input("(a) 3 0.5 5.3$"));
        assert_eq!("0.5", execute_input("(a) 3 0.5 (a)$"));
    }

    #[test]
    fn test_apply() {
        assert_eq!("1", execute_input("(1)@"));
        assert_eq!("4", execute_input("(1 1)@+2*"));
        assert_eq!("4", execute_input("(1 1+)@2*"));
        assert_eq!("1", execute_input("1 1@"));
        assert_eq!("0.1", execute_input("1 0.1@"));
    }

    #[test]
    fn test_apply_later() {
        assert_eq!("1", execute_input("(1)\\"));
        assert_eq!("4", execute_input("( 1 1+*)\\2"));
        assert_eq!("21", execute_input("(1)\\2"));
        assert_eq!("1", execute_input("1 1\\"));
        assert_eq!("0.1", execute_input("1 0.1\\"));
    }

    #[test]
    fn test_stacksize() {
        assert_eq!("1", execute_input("1#"));
        assert_eq!("3", execute_input("1 (a) 3.9#"));
        assert_eq!("0", execute_input("1 1$#"));
    }
}