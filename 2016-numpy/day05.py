from util import get_input
import numpy as np
from numpy.typing import NDArray
import hashlib


def solve_for(input: str):
    input = input.strip()

    counter = 0
    part1 = ""
    while len(part1) < 8:
        hash_input = f"{input}{counter}".encode()
        hash_result = hashlib.md5(hash_input).hexdigest()
        if hash_result.startswith("00000"):
            part1 += hash_result[5]
        counter += 1

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 5))
    print(f"Part 1: {part1} | Part 2: {part2}")
