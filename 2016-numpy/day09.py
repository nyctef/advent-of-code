import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().replace("\n", "")

    marker_re  =re.compile(r'\((\d+)x(\d+)\)')

    ptr = 0
    count = 0
    while ptr < len(input):
        print(input[ptr])
        if input[ptr] == '(':
            assert(marker := marker_re.search(input, ptr))
            print("marker", input[marker.start(): marker.end()])
            (length, reps) = tuple(int(x) for x in marker.groups())
            ptr = marker.end() + length
            count += length * reps
        else:
            count += 1
            ptr += 1

    part1 = count
    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 9))
    print(f"Part 1: {part1} | Part 2: {part2}")
