from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().splitlines()

    def to_dir(c: str) -> NDArray[np.int_]:
        match c:
            case "U":
                return np.array([0, -1])
            case "D":
                return np.array([0, 1])
            case "L":
                return np.array([-1, 0])
            case "R":
                return np.array([1, 0])
            case _:
                raise ValueError(f"Invalid direction: {c}")

    instructions = [[to_dir(c) for c in line] for line in input]

    keypad = """
123
456
789
""".strip()
    keypad = np.array([list(row) for row in keypad.splitlines()])

    print(keypad)

    part1 = ""
    pos = np.array([1, 1])
    for line in instructions:
        for instr in line:
            pos += instr
            pos = np.clip(pos, [0, 0], np.array(keypad.shape) - 1)
        part1 += keypad[pos[1], pos[0]]
        # this also works, but is a bit arcane (can it be simplified?)
        # part1 += keypad[tuple(pos[::-1].astype(int))]

    keypad2 = """
  1  
 234 
56789
 ABC 
  D  """.strip(
        "\n"
    )
    keypad2 = np.array([list(row) for row in keypad2.splitlines()])

    print(keypad2)
    part2 = ""
    pos = np.array([0, 2])
    for line in instructions:
        for instr in line:
            next_pos = pos + instr
            if not (
                0 <= next_pos[0] < keypad2.shape[1]
                and 0 <= next_pos[1] < keypad2.shape[0]
                and keypad2[next_pos[1], next_pos[0]] != " "
            ):
                continue
            pos = next_pos
        part2 += keypad2[pos[1], pos[0]]

    return (part1, part2)


def test_example_input():
    example = """
ULL
RRDDD
LURDL
UUUUD
"""
    (part1, part2) = solve_for(example)

    assert part1 == "1985"
    assert part2 == "5DB3"


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 2))
    print(f"Part 1: {part1} | Part 2: {part2}")
