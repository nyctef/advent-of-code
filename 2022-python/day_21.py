from itertools import chain
from pathlib import Path
from pprint import pprint
import re


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
    result = {}
    for line in input.splitlines():
        left, right = line.split(":")
        result[left] = right.strip()
    return result


def is_digits(s: str):
    return re.match(r"^\d+$", s)


def main():
    input = read_input_file("puzzle")
    lookup = parse_input(input)
    resolved = {}

    while lookup:
        for l, r in lookup.items():
            if is_digits(r):
                resolved[l] = r
                continue
            m = re.match(r"^(\w{4}).(.).(\w{4})$", r)
            assert m is not None
            left, op, right = m.group(1), m.group(2), m.group(3)
            rleft = resolved.get(left, None)
            rright = resolved.get(right, None)
            if rleft is not None and rright is not None:
                resolved[l] = eval(f"{rleft} {op} {rright}")

        for l in resolved.keys():
            lookup.pop(l, None)

    pprint(resolved)
    pprint(lookup)
    print(resolved["root"])


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
