from enum import Enum

# Parses an expression
# input: expression (as string) to evaluate
# context: optional current context (class Context)
def parse_expr(input, context = None):
    parts = input.split(".", 1) # Split name/expression + clean
    parts[0] = parts[0].strip()
    if len(parts) > 1 and parts[0].isidentifier():  # If <name> ’.’ <expr>
        # Create context for the <name> with context being both outerContext and argContext + disable self reference (since it's a function parameter, see Context)
        param_context = Context([(parts[0], None)], context, context, isParamContext=True)
        # Create function object with <name>, the remaining (parse result) of <expr> as content and the new context as context
        return Function(parts[0], parse_expr(parts[1], param_context), param_context)    
    else:   # Else it's an <apply>
        return parse_apply(input, context)

# Parses a (sequence of) <basic> expression(s)
# input: string to evaluate
# context: the current context 
def parse_apply(input, context):
    input = input.lstrip()
    separator = get_basic_end(input)    # Index of end of basic (+1)
    left = parse_basic(input[:separator], context)  # left = on the left of separator
    input = input[separator:].lstrip()  # input = on the right of separator
    while len(input) > 0:   # Apply basic (from left)
        separator = get_basic_end(input)    # Index of end of basic of the r.h.s. (+1)
        left = left.apply(input[:separator], context)   # Apply to l.h.s.
        input = input[separator:].lstrip()  # input = on the right of separator
    return left     # Return fully evaluated object

# Parses the different types of <basic>
# input: string to evaluate
# context: the current context 
def parse_basic(input, context):
    if input[0] == "(":
        return parse_expr(input[1:len(input)-1], context)   # Parse expression inbetween the parenthesis
    elif input[0] == "{" or input[0] == "[":    # Parse records
        isEager = (input[0] == "[")
        definitions = []
        input = input[1:len(input)-1].strip()   # Remove parenthesis
        sep = get_pair_end(input)       # Find end index of current pair
        while sep > 0:  # sep == 0 means no pair remaining in record
            eq_sep = input.find("=")    # Get first =
            if eq_sep == -1:
                exit("Error: = missing in record")
            name = input[:eq_sep].strip()  # Separate current pair into <name>=<content>
            content = input[eq_sep+1:sep].strip()
            definitions.append((name, content)) # Add pair to list of definitions
            input = input[sep+1:]   # Go to next pair
            sep = get_pair_end(input)
        if not isEager:
            return Context(definitions, context)    # Create new context with definitions and previous (outer) context
        else:
            return EagerContext(definitions, context)
    elif input[0].isnumeric() or input[0] == "-":   # Parse Integers
        return Int(int(input))
    elif input[0].isalpha(): 
        if input in {"plus", "minus", "mult", "div", "mod", "cond"}:    # Predefined function
            return PredefFunc(input)
        elif context != None:       # Function parameter or <name> from record definition (both handled as part of context)
            return context.eval_name(input)
        else:
            exit(f"Error: Unknown literal {input}")
    else:
        exit(f"Error: Unexpected symbol {input[0]}")

# Returns end index of <basic> (+1)
def get_basic_end(input):
    if input[0] == "(":     # Count brackets
        return get_bracket_end(input, "(", ")")
    elif input[0] == "{":   # Count brackets
        return get_bracket_end(input, "{", "}")
    # TODO: remove if eager records don't work
    elif input[0] == "[":   # Count brackets
        return get_bracket_end(input, "[", "]")
    elif input[0].isnumeric() or input[0] == "-":
        if input[0] == "-" and not input[1].isnumeric():    # Minus followed by no integer
            exit("Error: Invalid number")
        for index in range(1, len(input)):  # Return index of first non-numeric character
            if not input[index].isnumeric():
                return index
        return len(input)   # If all numeric, return length of input
    elif input[0].isalpha():
        for index in range(1, len(input)):
            if not input[index].isalnum() and not input[index] == "_":
                return index
        return len(input)
    else:
        exit(f"Error: Unexpected symbol {input[0]}")

# Returns index of the corresponding closing bracket + 1
# Input: string, starts with open_bracket
def get_bracket_end(input, open_bracket, close_bracket):
    open = 0
    for index in range(len(input)):
        if input[index] == open_bracket:
            open += 1
        if input[index] == close_bracket:
            open -= 1
            if open == 0:
                return index+1
    exit(f"Error: Missing closing bracket {close_bracket} in {input}")

# Returns end index of the current pair (next comma or the index of the final closing parenthesis) (+1)
def get_pair_end(input):
    open = 0
    for index in range(len(input)):
        if open == 0 and input[index] == ",":   # End of pair (no open parenthesis)
            return index
        elif input[index] == "{" or input[index] == "[":
            open += 1
        elif input[index] == "}" or input[index] == "]":
            open -= 1
    return len(input)

class ParseType(Enum):
    FUNCTION = 1
    INT = 2
    PARAM = 3
    RECORD = 4
    APPLY = 5
    PREDEF_FUNC = 6

# Represents a function
# param: the function parameter
# content: the content of the function body
# context: the context of the function parameter 
class Function:
    def __init__(self, param, content, context):
        self.type = ParseType.FUNCTION
        self.param = param
        self.content = content
        self.context = context

    # Returns result of the function applied to the input
    # self.context of Function is always the context containing the param of the function => update context to param=input
    # input: parameter for the function
    # context: context of parameter (input)
    def apply(self, input, context):
        self.context.definitions[0] = (self.context.definitions[0][0], input)   # Replace value of tuple of parameter definition with input
        self.context.outerContext = context     # Set outer context to context of parameter
        return self.content.update(self.param)  # Evaluate function under new context
    
    # Evaluate function under new context
    def update(self, param):
        # no update if another function masks the parameter
        if param != self.param:
            self.content = self.content.update(param)
        return self
    
    def __str__(self):
        return f"({self.param}.{self.content})"

# Integer type of our language
class Int:
    def __init__(self, value):
        self.type = ParseType.INT
        self.value = value

    # Returns the int (nothing happens)
    def apply(self, input, context):
        return self
    
    # Returns the int (nothing happens)
    def update(self, param):
        return self
    
    def __str__(self):
        return f"{self.value}"

# Function parameter that does not have a value yet
class Param:
    def __init__(self, name, context):
        self.type = ParseType.PARAM
        self.name = name
        self.context = context

    # Cannot evaluate yet => return Apply object
    def apply(self, input, context):
        return Apply(self, input, context)
    
    # Returns value for the corresponding definition if param matches the current parameter
    # Otherwise nothing happens
    def update(self, param):
        if param == self.name:
            return self.context.eval_name(param)    
        else:
            return self
    
    def __str__(self):
        return f"{self.name}"

# Temporary placeholder for objects that received an apply but cannot be evaluated yet (e.g. parameters)
class Apply:
    def __init__(self, left, right, rcontext):
        self.type = ParseType.APPLY
        self.left = left
        self.right = right
        self.rcontext = rcontext

    # Still cannot be evaluated
    def apply(self, input, context):
        self.rcontext = context # TODO: Check if this line is correct!
        return Apply(self, input, context)
    
    # Update on left side and try apply.
    # Either returns an evaluated object, or an Apply object 
    def update(self, param):
        self.left = self.left.update(param)
        return self.left.apply(self.right, self.rcontext)
    
    def __str__(self):
        return f"({self.left} ({self.rcontext} {self.right}))"

# Set of predefined functions according to the language specification
class PredefFunc:
    def __init__(self, keyword):
        self.type = ParseType.PREDEF_FUNC
        self.keyword = keyword
        match self.keyword:
            case "plus":
                self.num_params = 2
            case "minus":
                self.num_params = 2
            case "mult":
                self.num_params = 2
            case "div":
                self.num_params = 2
            case "mod":
                self.num_params = 2
            case "cond":
                self.num_params = 3
            case _:
                exit(f"Error: Undefined function keyword {keyword}")
        self.collected_params = []
        self.param_context = None

    # Add new parameter to the collected_params list and potentially evaluate function  
    def apply(self, input, context):
        self.param_context = context    # All params have the same context
        if len(self.collected_params) == self.num_params:   # If already full, discard parameter
            return self
        if self.keyword != "cond" or len(self.collected_params) == 0:   # Integer function or first param of cond
            self.collected_params.append(parse_expr(input, context))    # Store parsed/evaluated parameter
        else:
            self.collected_params.append(input) # Store unparsed parameter 
        return self.try_compute()
        
    # Check if function can be evaluated
    def try_compute(self):
        if len(self.collected_params) == self.num_params:   # If all parameters available
            if self.keyword in {"plus", "minus", "mult", "div", "mod"}: # Integer function
                for param in self.collected_params:     # Return if any parameter is not an int
                    if param.type != ParseType.INT:
                        return self
                match self.keyword:     # Execute the predefined functions on the two integer parameters
                    case "plus":
                        return Int(self.collected_params[0].value + self.collected_params[1].value)
                    case "minus":
                        return Int(self.collected_params[0].value - self.collected_params[1].value)
                    case "mult":
                        return Int(self.collected_params[0].value * self.collected_params[1].value)
                    case "div":
                        if self.collected_params[1].value == 0:
                            exit("Error: Division by 0 not allowed")
                        return Int(int(self.collected_params[0].value / self.collected_params[1].value))
                    case "mod":
                        return Int(self.collected_params[0].value % self.collected_params[1].value)
            elif self.keyword == "cond":
                if self.collected_params[0].type == ParseType.INT:      # If first param is <Integer>, then execute second (if non-zero) or third parameter (if 0)
                    if self.collected_params[0].value != 0: 
                        return parse_expr(self.collected_params[1], self.param_context)
                    else:
                        return parse_expr(self.collected_params[2], self.param_context)
                elif self.collected_params[0].type == ParseType.RECORD: # If first param is a record, then execute second (if non-empty) or third parameter (if empty)
                    if len(self.collected_params[0].definitions) > 0:
                        return parse_expr(self.collected_params[1], self.param_context)
                    else:
                        return parse_expr(self.collected_params[2], self.param_context)
                else:
                    return self
        return self

    # Propagates update to correct parameters
    def update(self, param):
        if self.keyword != "cond":  # Integer function: Propagate update to all collected parameters
            for i in range(len(self.collected_params)):
                self.collected_params[i] = self.collected_params[i].update(param)
        elif len(self.collected_params) > 0:    # Cond function: Propagate update to first collected parameter
            self.collected_params[0] = self.collected_params[0].update(param)
        return self.try_compute()   # Check if function can be computed
    
    def __str__(self):
        delim = " "
        context_string = str(self.param_context) + " "
        if self.keyword in {"plus", "minus", "mult", "div", "mod"} or self.param_context == None or context_string == "{} " or (self.keyword == "cond" and len(self.collected_params) < 2):
            return f"({self.keyword} {delim.join([param.__str__() for param in self.collected_params])})"
        else:
            return f"({context_string}({self.keyword} {delim.join([param.__str__() for param in self.collected_params])}))"

# Context under which expressions are evaluated (list of (name=value), where values can be any expressions)
class Context:
    # definitions:  list of (name, value) tuples
    # outerContext: context of potential previous records (e.g.: {x=5}({x=1}{y=x} (plus x y)) evaluates to 6)
    #               implemented since variables on the same level might have different context: 
    #               e.g.: in ({a=..., b=..., c=...} (b c)) b doesn't have any context for c, but c does
    # argContext:   next context in the linked list: e.g. for example from before: context {y={x=1}x} has context {x=5} as next element in linked list
    # isParamContext: when context is used for function parameters we might get self referencing parameters that create an infinite loop
    #               e.g.: {x=5}((x.x)x) evaluates to {x=5}({x=x} x), evaluating {x=x} would lead to an infinite loop.
    #               Hence, for function parameters, self reference of contextes is not allowed (it is however allowed for records itself, e.g. list definition)
    def __init__(self, definitions, outerContext=None, argContext = None, isParamContext = False):
        self.type = ParseType.RECORD
        self.definitions = definitions
        self.outerContext = outerContext
        self.argContext = argContext
        self.isParamContext = isParamContext

    # Set context as argContext (next context) + evaluate input with self as context 
    def apply(self, input, context):
        self.argContext = context
        return parse_expr(input, self)
    
    # Parameters are stored in separate contexts and context entries are evaluated lazy -> no update necessary
    def update(self, param):        
        return self

    # Returns value corresponding to the given name in this context
    def eval_name(self, name):
        for i in range(len(self.definitions)-1, -1, -1):
            if self.definitions[i][0] == name:  # Name found
                if self.definitions[i][1] == None:  # Parameter without value => return Param object
                    return Param(name, self)
                else:   # Hit with value
                    if not self.isParamContext: # Create new context containing all definitions left of (and including) the current definition + evaluate definition
                        return parse_expr(self.definitions[i][1], Context(self.definitions[:i+1], self.outerContext, self.outerContext))
                    else:   # Parameter context => Contains only 1 definition => evaluate under outerContext
                        return parse_expr(self.definitions[i][1], self.outerContext)
        if self.argContext != None: # Try to find in argContexts
            return self.argContext.eval_name(name)
        else:
            exit(f"Error: Unknown literal {name}")  # Literal not found anywhere in context

    def __str__(self):
        return f"{{{self.get_definitions_string(set())}}}"
    
    # Inner output formatting
    def get_definitions_string(self, skip_set):
        output = ""
        outerContextDefStr = ""
        delim = ", "
        if self.outerContext != None and not (self.isParamContext and self.definitions[0][1] == None):
            outerContextDefStr = self.outerContext.get_definitions_string(set())
        def_list = []
        for i in range(len(self.definitions)-1, -1, -1):
            if self.definitions[i][0] not in skip_set and self.definitions[i][1] != None:
                prev_def_string = delim.join([f"{definition[0]} = {definition[1]}" for definition in self.definitions[0:i]])
                if len(outerContextDefStr) > 0 and len(prev_def_string) > 0:
                    prev_def_string = "{" + outerContextDefStr + ", " + prev_def_string + "}"
                elif len(outerContextDefStr) > 0 or len(prev_def_string) > 0:
                    prev_def_string = "{" + outerContextDefStr + prev_def_string + "}"
                def_list.append(f"{self.definitions[i][0]} = {prev_def_string} ({self.definitions[i][1]})")
            skip_set.add(self.definitions[i][0])

        if self.argContext != None:
            output = self.argContext.get_definitions_string(skip_set)
            if len(output) > 0 and len(def_list) > 0:
                output += ", "
        output += delim.join(reversed(def_list))
        return output


# TODO: Currently not working, leads to infinite recursion
class EagerContext:
    def __init__(self, definitions, outerContext=None, argContext = None, parsed = False, self_ref_name = None):
        self.type = ParseType.RECORD
        self.definitions = definitions
        self.outerContext = outerContext
        if not parsed:
            for i in range(len(self.definitions)):
                value_str = self.definitions[i][1]
                self.definitions[i] = (self.definitions[i][0], None)
                sub_context = EagerContext(self.definitions[:i+1], self.outerContext, self.outerContext, parsed = True, self_ref_name=self.definitions[i][0])
                self.definitions[i] = (self.definitions[i][0], parse_expr(value_str, sub_context))
                sub_context.definitions[i] = self.definitions[i]
        # outerContext: context of record
        # argcontext: context of argument of record (linked list of applicable records)
        self.argContext = argContext
        self.self_ref_name = self_ref_name

    def apply(self, input, context):
        self.argContext = context
        return parse_expr(input, self)
    
    def update(self, param):
        for definition in self.definitions:
            definition[1] = definition[1].update(param)
        return self

    def eval_name(self, name):
        for i in range(len(self.definitions)-1, -1, -1):
            if self.definitions[i][0] == name:
                return self.definitions[i][1]
        if self.argContext != None:
            return self.argContext.eval_name(name)
        else:
            exit(f"Error: Unknown literal {name}")

    def __str__(self):
        return f"{{{self.get_definitions_string()}}}"
    
    def get_definitions_string(self, skip_set = set()):
        output = ""
        delim = ", "
        def_list = []
        end_index = len(self.definitions)-1
        if self.definitions[end_index][0] == self.self_ref_name:
            end_index -= 1
        for i in range(end_index, -1, -1):
            if self.definitions[i][0] not in skip_set and self.definitions[i][1] != None:
                def_list.append(f"{self.definitions[i][0]} = {self.definitions[i][1]}")
            skip_set.add(self.definitions[i][0])

        if self.argContext != None:
            output = self.argContext.get_definitions_string(skip_set)
            if len(output) > 0 and len(def_list) > 0:
                output += ", "
        output += delim.join(reversed(def_list))
        return output

