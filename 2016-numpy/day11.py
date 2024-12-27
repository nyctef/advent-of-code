import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()
    state = []
    for line in lines:
        floor = []
        for mc in re.finditer(r"(\w+)-compatible microchip", line):
            floor.append(f"mc {mc.group(1)}")
        for gen in re.finditer(r"(\w+) generator", line):
            floor.append(f"gen {gen.group(1)}")
        state.append(floor)

    print(state)

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
    (part1, part2) = solve_for(get_input(2016, 11))
    print(f"Part 1: {part1} | Part 2: {part2}")
