import unittest
from interpreter import parse_expr

class TestInterpreter(unittest.TestCase):
    def test_parse(self):
        self.assertEqual(str(parse_expr("x.x")), "(x.x)")

    def test_function_apply(self):
        self.assertEqual(str(parse_expr("(x.x) 1")), "1")
        self.assertEqual(str(parse_expr("(x.x) (plus 1)")), "(plus 1)")
        self.assertEqual(str(parse_expr("(x.x) (minus 1) 3")), "-2")
        self.assertEqual(str(parse_expr("(x.x ((x.x) 1) ((x.x) 3)) minus")), "-2")
        self.assertEqual(str(parse_expr("(x.y.z.x ((x.x) y) ((x.x) z)) minus 1 3")), "-2")

    def test_int_apply(self):
        self.assertEqual(str(parse_expr("5 abc")), "5")
        self.assertEqual(str(parse_expr("-12345 plus 1")), "-12345")

    def test_predef_func(self):
        self.assertEqual(str(parse_expr("plus -1 1")), "0")
        self.assertEqual(str(parse_expr("minus 10 5")), "5")
        self.assertEqual(str(parse_expr("mult 10 5")), "50")
        self.assertEqual(str(parse_expr("div 10 5")), "2")
        self.assertEqual(str(parse_expr("div 10 4")), "2")
        self.assertEqual(str(parse_expr("mod 10 4")), "2")
        self.assertEqual(str(parse_expr("cond 1 5 0")), "5")
        self.assertEqual(str(parse_expr("cond 43278654 5 0")), "5")
        self.assertEqual(str(parse_expr("cond {a=2} 5 0")), "5")
        self.assertEqual(str(parse_expr("cond (x.x) 5 0")), "(cond (x.x) 5 0)")
        self.assertEqual(str(parse_expr("cond 0 5 0")), "0")
        self.assertEqual(str(parse_expr("cond {} 5 0")), "0")
        self.assertEqual(str(parse_expr("y.{x=5} (cond ({x=0} (y x)))")), "(y.(cond (y ({x = {x =  (5)} (0)} x))))")
        self.assertEqual(str(parse_expr("y.{x=5} (cond ({x=plus 1 x} (y x)))")), "(y.(cond (y ({x = {x =  (5)} (plus 1 x)} x))))")

    def test_record(self):
        self.assertEqual(str(parse_expr("{x=5, y=x}(x.x y)")), "(x.(x ({y = {x = 5} (x)} y)))")
        self.assertEqual(str(parse_expr("{x=5} x")), "5")
        self.assertEqual(str(parse_expr("{x=1, y=plus x 1, x=plus y 1, y=plus x 1} y")), "4")
        self.assertEqual(str(parse_expr("{x=1} (x.x)")), "(x.x)")
        self.assertEqual(str(parse_expr("{x=5, y=x}(x.{x=3}(x y))")), "(x.3)")

    def test_lists(self):
        list_record = str("{list=c.f.x.cond (c x) { val=x, nxt=list c f (f x) } {} , " \
            "reduce=f.x.l.cond l (f (reduce f x (l nxt)) (l val)) x , " \
            "range=a.b.list (x.minus b x) (x.plus 1 x) a , " \
            "sum=l.reduce (x.y.plus x y) 0 l, " \
            # get: get element at index i of list l, i has to be within range
            "get=l.i.cond i (get (l nxt) (minus i 1)) (l val)," \
            "len=l.reduce (x.y.plus 1 x) 0 l}")
        self.assertEqual(str(parse_expr(f"{list_record} range 2 3 val")), "2")
        self.assertEqual(str(parse_expr(f"{list_record} (sum (range 1 10))")), "45")
        self.assertEqual(str(parse_expr(f"{list_record} (get (range 1 10) 0)")), "1")
        self.assertEqual(str(parse_expr(f"{list_record} (get (range 1 10) 3)")), "4")
        self.assertEqual(str(parse_expr(f"{list_record} (len (range 1 10))")), "9")

if __name__ == '__main__':
    unittest.main()