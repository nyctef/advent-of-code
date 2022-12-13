from itertools import zip_longest
from pathlib import Path
from pprint import pprint
from typing import List


input_file = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]

[[1, 2], 3]
[[1, 2], 0]
"""

input_file = Path("input/13-1.txt").read_text()

inputs = input_file.split("\n\n")
inputs = [tuple(map(eval, x.splitlines())) for x in inputs]

Foo = List["int | Foo"]


def nothing(*args):
    pass


log = nothing


def compare(left: Foo, right: Foo):
    for (l, r) in zip_longest(left, right, fillvalue=None):
        if l is None:
            log("left side ran out of items, so correct")
            return "CORRECT"
        elif r is None:
            log("right side ran out of items, so not in right order")
            return "WRONG"
        elif isinstance(l, int) and isinstance(r, int):
            if r < l:
                log(f"{r=} is less than {l=}, so wrong order")
                return "WRONG"
            elif l < r:
                log(f"{l=} < {r=}, so right order")
                return "CORRECT"
            else:
                log(f"{l=} = {r=}, so contining")
        elif isinstance(l, list) and isinstance(r, list):
            log(f"recursing into comparing {l=} and {r=}")
            r = compare(l, r)
            if r is not "EQUAL":
                return r
        elif isinstance(l, int) and isinstance(r, list):
            log(f"recursing into comparing {[l]=} and {r=}")
            r = compare([l], r)
            if r is not "EQUAL":
                return r
        elif isinstance(l, list) and isinstance(r, int):
            log(f"recursing into comparing {l=} and {[r]=}")
            r = compare(l, [r])
            if r is not "EQUAL":
                return r
        else:
            raise Exception((l, r))
    return "EQUAL"


pprint(inputs)

result = 0

for (i, (l, r)) in enumerate(inputs, start=1):
    r = compare(l, r)
    print((i, r))
    if r != "WRONG":
        result += i


def debug(i):
    print(i, inputs[i - 1], compare(inputs[i - 1][0], inputs[i - 1][1]))


# debug(2)
# print(list(zip_longest(inputs[4][0], inputs[4][1], fillvalue=None)))

print(result)
