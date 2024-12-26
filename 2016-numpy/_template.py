from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    part1 = ""
    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, __DAY__))
    print(f"Part 1: {part1} | Part 2: {part2}")
