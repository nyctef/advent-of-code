from itertools import zip_longest
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
"""

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
            return True
        elif r is None:
            log("right side ran out of items, so not in right order")
            return False
        elif isinstance(l, int) and isinstance(r, int):
            if r < l:
                log(f"{r=} is less than {l=}, so wrong order")
                return False
            else:
                log(f"{l=} <= {r=}, so right order")
        elif isinstance(l, list) and isinstance(r, list):
            log(f"recursing into comparing {l=} and {r=}")
            if not compare(l, r):
                return False
        elif isinstance(l, int) and isinstance(r, list):
            log(f"recursing into comparing {[l]=} and {r=}")
            return compare([l], r)
        elif isinstance(l, list) and isinstance(r, int):
            log(f"recursing into comparing {l=} and {[r]=}")
            return compare(l, [r])
        else:
            raise Exception((l, r))
    return True


pprint(inputs)

for (i, (l, r)) in enumerate(inputs, start=1):
    print((i, compare(l, r)))

# print((8, inputs[7], compare(inputs[7][0], inputs[7][1])))
# print(list(zip_longest(inputs[4][0], inputs[4][1], fillvalue=None)))
