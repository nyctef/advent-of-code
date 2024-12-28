import itertools
from util import get_input
import numpy as np
from numpy.typing import NDArray


def grouper(iterable, n):
    """
    based on  https://docs.python.org/3/library/itertools.html#itertools-recipes
    """
    # this works because we only end up with one copy of the iterator shared across
    # each of the elements in `iterators`. zip() gets the next element of each iterator
    # in turn (which ends up consuming the next `n` items) and returns them as a tuple
    iterators = [iter(iterable)] * n
    return zip(*iterators, strict=True)


def expand(a: str):
    b = a
    b = "".join(reversed(b))
    b = "".join("1" if x == "0" else "0" for x in b)
    return a + "0" + b


def do_checksum(a: str):
    assert len(a) % 2 == 0
    result = []
    for l, r in grouper(a, 2):
        result.append("1" if l == r else "0")

    return "".join(result)


def solve_for(input: str, length: int):
    seed = input.strip()

    data = seed
    while len(data) < length:
        data = expand(data)

    data = data[:length]
    checksum = do_checksum(data)
    while (len(checksum) & 1) == 0:
        checksum = do_checksum(checksum)

    return checksum


def test_example_input():
    part1 = solve_for("10000", 20)

    assert part1 == "01100"


if __name__ == "__main__":
    part1 = solve_for(get_input(2016, 16), 272)
    part2 = "???"
    print(f"Part 1: {part1} | Part 2: {part2}")
