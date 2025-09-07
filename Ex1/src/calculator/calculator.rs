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
    /// Creates a new instance of the calculator, initializes registers:
    /// Register a: Welcome message + executes register b
    /// Register b: Loop that queries for user input
    /// Register c: Computes the ultimate answer to life, the universe, and everything
    /// Register d: Reverses an input string
    /// Register e: Reverses individual words of an input string and counts words, letters, digits, white-spaces and special characters
    pub fn new() -> Self {
        let mut registers = HashMap::new();
        for c in 'A'..='Z' {
            registers.insert(c, Operand::String(String::new()));
        }
        for c in 'a'..='z' {
            registers.insert(c, Operand::String(String::new()));
        }
        registers.insert('a', Operand::String(String::from("(Welcome to our awesome calculator!\n)\"b@"))); // Welcome message
        registers.insert('b', Operand::String(String::from("
(What do you want to calculate?\n)
\"\'@
(\n)\"
()b@")));   // Repeated command prompt
        registers.insert('c', Operand::Integer(42));    // Constant for testing
        registers.insert('d', Operand::String(String::from("(3!3$3!1%3!*2$1 4!-3$2!_(4!@)(3$1$)4!4$1+$@) () 4!4$ 4!@")));   // Reversal of an input string
        
        let mut program = String::new();
// begin loop code (code for loop is pushed on data stack)
    // stack contents: full input string, loop code, 5x counters, input string, result buffer, word buffer
    // begin if input string not empty
        // copy input to front, get first letter
        program += "((4!1% ";
        // stack contents: full input string, loop code, 5x counters, input string, result buffer, word buffer, first letter
        // if letter condition: (x>64 & x<91) | (x>96 & x<123)
        program += "2!64>3!91<&3!96>4!123<&| ";
        // begin if letter false
            // if digit condition: x>47 & x<58
        program += "(2!47>3!58<& ";
            // begin if digit false
                // if whitespace condition: x=32
        program += "(2!32= ";
                // begin if whitespace false (special char)
                    // increment special char counter
        program += "(10!10$10!10$10!10$10!10$10!1+10$10!10$10!10$10!10$10!10$) ";
                // end if whitespace false
                // begin if whitespace true
                    //increment whitespace counter
        program += "(10!10$10!10$10!10$10!1+10$10!10$10!10$10!10$10!10$10!10$) ";
                // end if whitespace true
                // take if whitespace bool from data stack, add 1, delete wrong path from stack (1$ if not whitespace, 2$ if whitespace), execute remaining code path
        program += "4!4$1+$@ ";
                // add first letter to back of word buffer, add word buffer with first letter to result buffer, push new empty word buffer
        program += "*+()) ";
            // end if digit false
            // begin if digit true
                // analogous to (if letter true): update word count if word buffer empty, increment digit counter, append first letter to front of word buffer
        program += "(10! 4!_+ 10$10!10$10!1+10$10!10$10!10$10!10$10!10$9!9$10!10$*) ";
            // end if digit true
            // take if digit bool from data stack, add 1, delete wrong path from stack (1$ if not digit, 2$ if digit), execute remaining code path
        program += "4!4$1+$@) ";
        // end if letter false
        // begin if letter true
            // update word count: if word buffer empty add 1 to word count, otherwise add 0, old word count deleted
        program += "(10! 4!_+ 10$ ";
            // update letter count: add 1, delete old letter count
        program += "10!1+10$ ";
            // copy and delete remaining 3x counters, input string, result buffer, first letter, word buffer
            // and append first letter to the front of word buffer (append to front for word reversal)
        program += "10!10$10!10$10!10$10!10$10!10$9!9$10!10$*) ";
        // end if letter true
        // take if letter bool from data stack, add 1, delete wrong path from stack (1$ if not letter, 2$ if letter), execute remaining code path
        program += "4!4$1+$@ ";
        // move input string to top of data stack and delete first letter
        program += "1 5!5$-";
        // stack contents: full input string, loop code, 5x counters, result buffer, word buffer, updated input string
        // bring result and word buffer to front, bring loop code to front and execute
        program += "4!4$4!4$10!@) ";
        // stack contents: full input string, loop code, 5x counters, input string, result buffer, word buffer
    // end if input string not empty
    // begin if input string empty
        // add word buffer to result buffer, delete input string and loop code
        program += "(+2$7$) ";
    // end if input string empty
    // if input string empty condition: bring input string to front, check if empty, add 1, delete wrong path from stack (1$ if not empty, 2$ if empty), execute remaining code path
        program += "6!_1+$@) ";
// end loop code
// initialize counters: word count, letter count, digit count, whitespace count, special characters count
        program += "0 0 0 0 0 ";
// copy input string from first position
        program += "8! ";
// init result buffer, word buffer
        program += "() () ";
// get loop code from data stack and execute
        program += "10!@ ";
// Cleanup + output
        program += "7$(Reversed words: )\"\"(\nNumber of special characters: )4!(\nNumber of white-spaces: )7!(\nNumber of digits: )10!(\nNumber of letters: )13!(\nNumber of words: )\"\"\"\"\"\"\"\"\"\"(\n)\"1$1$1$1$ "; 

        registers.insert('e', Operand::String(program));    // Reversal of all words in an input string according to the specification, including word counts
                    
        Calculator { 
            commands: VecDeque::new(),
            operation_mode: 0,
            data: Vec::new(),
            registers,
        }
    }

    /// Turns on the calculator and queries for user input
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
            // eprintln!("{:?} - {}", self.data, self.commands.iter().collect::<String>());
            if res.is_err() {
                eprintln!("Error: {}\nShutting down...", res.err().unwrap());
                return;
            }
        }
    }

    /// Handle next command character when in integer construction mode
    /// next_command: next command character on the command stream
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

    /// Handle next command character when in decimal construction mode
    /// next_command: next command character on the command stream
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

    /// Handle next command character when in string construction mode
    /// next_command: next command character on the command stream
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

    /// Handle next command character when in execution mode
    /// next_command: next command character on the command stream
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
            '!' => {    // Copy (1-indexed, includes the index itself)
                if self.data.is_empty() {
                    return Err(String::from("Data stack cannot be empty for copy operator!"));
                }
                match self.data.last().unwrap() {
                    Operand::Integer(i) => {
                        let len = self.data.len();
                        if *i as usize > len || *i <= 0 {
                            return Ok(());
                        }

                        let new = self.data[len - *i as usize].clone();
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

/// Helper function for testing the internal workings of the calculator
/// input: the input string for the calculator
/// returns string representation of item on top of the datastack if non-empty, empty string otherwise 
pub fn execute_input(input: &str) -> String {
    let mut calculator = Calculator::new();
    calculator.commands.extend(input.chars());
    calculator.execute_commands();
    match calculator.data.pop() {
        Some(x) => x.to_string(),
        None => String::new()
    }
}


/// Tests the implementation of all predefined operations for correctness
#[cfg(test)]
mod functionality_tests {
    use super::*;

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
        assert_eq!("1", execute_input("(a) 1!"));
        assert_eq!("a", execute_input("(a) 2!"));
        assert_eq!("3", execute_input("(a) 3 0.5 3!"));
        assert_eq!("0.5", execute_input("(a) 3 0.5 2! 3!"));
        assert_eq!("3", execute_input("(a) 3!")); // barely out of range
        assert_eq!("0", execute_input("(a) 0!")); // barely out of range
        assert_eq!("6", execute_input("(a) 3 0.5 6!")); // out of range
        assert_eq!("5.3", execute_input("(a) 3 0.5 5.3!")); // float
        assert_eq!("a", execute_input("(a) 3 0.5 (a)!"));   // string
    }

    #[test]
    fn test_delete() {
        assert_eq!("", execute_input("(a) 1$"));
        assert_eq!("a", execute_input("(a) 3 1$"));
        assert_eq!("3", execute_input("(a) 3 2$"));
        assert_eq!("a", execute_input("(a) 3 0.5 2$ 3!"));
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

    #[test]
    fn test_register_access() {
        assert_eq!("42", execute_input("c@"));
    }

    #[test]
    fn test_conditional_example() {
        assert_eq!("8", execute_input("1(8)(9~)(4!4$_1+$@)@"));
    }

    #[test]
    fn test_factorial() {
        assert_eq!("2", execute_input("2(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$"));    // 2!
        assert_eq!("6", execute_input("3(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$"));    // 3!
        assert_eq!("24", execute_input("4(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$"));   // 4!
        assert_eq!("3628800", execute_input("10(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$")); // 10!        
    }

    #[test]
    fn test_word_reversal() {
        assert_eq!("cba", execute_input("(abc)d@"));
        assert_eq!("2ksbt4", execute_input("(4tbsk2)d@"));
        assert_eq!("2ks.bt4", execute_input("(4tb.sk2)d@"));
        assert_eq!("Hello World!", execute_input("(!dlroW olleH)d@"));
    }
}