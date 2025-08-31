import os

def parse_input(args):
    if len(args) < 2:
        print("Please supply an input file or string.")
        return None
    
    # If argument is not a file, read it as input string
    if not os.path.exists(args[1]):
        return args[1]
    
    # Otherwise read the file contents
    with open(args[1], 'r') as file:
        return file.read()