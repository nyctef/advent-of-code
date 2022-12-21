from collections import defaultdict
from itertools import chain
from pathlib import Path
from pprint import pprint
import re
import z3


def read_input_file(name: str):
    match name:
        case "puzzle":
            return Path("input/21-1.txt").read_text()
        case "example":
            return """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
"""
        case other:
            raise Exception(other)


def parse_input(input: str):
    result: dict[str, str] = {}
    for line in input.splitlines():
        left, right = line.split(":")
        result[left] = right.strip()
    return result


def is_digits(s: str):
    return re.match(r"^\d+$", s)


def main():
    input = read_input_file("puzzle")
    lookup = parse_input(input)

    vars = {}

    def get_var(name: str):
        return vars.get(name, z3.Real(name))

    humn = get_var("humn")
    root = get_var("root")

    s = z3.Solver()

    # s.add(humn == z3.IntVal(42))
    for l, r in lookup.items():
        if l == "humn":
            # we're told to ignore this one for part 2
            continue
        target_var = get_var(l)
        if is_digits(r):
            val = int(r)
            s.add(target_var == z3.IntVal(val))
        else:
            m = re.match(r"^(\w{4}).(.).(\w{4})$", r)
            assert m is not None
            left, op, right = m.group(1), m.group(2), m.group(3)
            lvar = get_var(left)
            rvar = get_var(right)
            if l == "root":
                s.add(target_var == lvar)
                s.add(target_var == rvar)
                s.add(lvar == rvar)
                continue
            match op:
                case "+":
                    s.add(target_var == lvar + rvar)
                case "-":
                    s.add(target_var == lvar - rvar)
                case "*":
                    s.add(target_var == lvar * rvar)
                case "/":
                    s.add(target_var == lvar / rvar)
                case other:
                    raise Exception(f"unknown op {other}")

    print(s.sexpr())
    print(s.check())
    m = s.model()
    print(f"{m[humn]=} {m[root]=}")


"""
    q = []
    q.append("root")
    while q:
        n = q.pop()
        if is_digits(n):
            continue
        m = re.match(r"^(\w{4}).(.).(\w{4})$", n)
        left, op, right = 
"""


# input = input_file.splitlines()
# lefts = [x.split(":")[0] for x in input]
# lefts = [x for x in lefts if x != "root"]

# print(f"{len(lefts)=}")
# rights = [x.split(":")[1] for x in input]
# rights = [re.match(r"(\w{4})...(\w{4})", x.strip()) for x in rights]
# rights = [(x.group(1), x.group(2)) for x in rights if x is not None]
# rights = list(chain.from_iterable(rights))
# print(f"{rights[0: 10]=}")
# print(len(rights))

main()
