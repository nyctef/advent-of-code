import re
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
            print(hash_result[5], end="", flush=True)
        counter += 1
    print()

    part2 = list("        ")
    counter = 0
    while " " in part2:
        counter += 1
        hash_input = f"{input}{counter}".encode()
        hash_result = hashlib.md5(hash_input).hexdigest()
        if hash_result.startswith("00000"):
            position = hash_result[5]
            char = hash_result[6]
            if not position.isdigit():
                continue
            position = int(position)
            if position < 0 or position > 7 or part2[position] != " ":
                continue
            part2[position] = char
            print("".join(part2))

    return (part1, "".join(part2))


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 5))
    print(f"Part 1: {part1} | Part 2: {part2}")
