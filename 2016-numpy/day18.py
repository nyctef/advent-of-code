import functools
from util import get_input
import numpy as np
from numpy.typing import NDArray


@functools.lru_cache(None)
def iterate(input: str):
    result = []
    for i in range(len(input)):
        left_trap = i >= 1 and input[i - 1] == "^"
        center_trap = input[i] == "^"
        right_trap = i < len(input) - 1 and input[i + 1] == "^"

        """
        XX0
        0XX
        X00
        00X

        0X0
        XXX
        X0X
        OOO
        """

        group = (left_trap, center_trap, right_trap)
        if group in [
            (False, True, False),
            (True, True, True),
            (True, False, True),
            (False, False, False),
        ]:
            result.append(".")
        else:
            result.append("^")

    return "".join(result)


def count_safe(input: str):
    return sum(1 for x in input if x == ".")


def solve_for(input: str, target):
    first_row = input.strip()

    row = first_row
    part1 = count_safe(row)
    for i in range(target - 1):
        # print(row)
        row = iterate(row)
        part1 += count_safe(row)
    return part1


def test_example_input():
    example = """
.^^.^.^^^^
"""
    part1 = solve_for(example, 10)

    assert part1 == 38


if __name__ == "__main__":
    part1 = solve_for(get_input(2016, 18), 40)
    part2 = solve_for(get_input(2016, 18), 400_000)
    print(f"Part 1: {part1} | Part 2: {part2}")
