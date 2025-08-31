import sys
from parser import parse_input
from interpreter import parse_expr

def main():
    # Parse input from commandline or file
    code = parse_input(sys.argv)
    if not code:
        return
    
    # Evaluate expressions + output result
    print(parse_expr(code))

if __name__ == "__main__":
    main()