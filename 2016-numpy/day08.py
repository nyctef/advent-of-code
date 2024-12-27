import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    display = np.full((6, 50), " ", dtype="U1")

    for instr in lines:
        if m := re.match(r"^rect (\d+)x(\d+)$", instr):
            (w, h) = tuple(int(x) for x in m.groups())
            display[:h, :w] = "#"
        elif m := re.match(r"^rotate row y=(\d+) by (\d+)$", instr):
            (row, rot) = tuple(int(x) for x in m.groups())
            display[row] = np.roll(display[row], rot)
        elif m := re.match(r"^rotate column x=(\d+) by (\d+)$", instr):
            (col, rot) = tuple(int(x) for x in m.groups())
            display[:, col] = np.roll(display.T[col], rot)
        else:
            raise Exception(f"unknown instr {instr}")

    part1 = (display == "#").sum()

    part2 = "\n"
    for r in range(display.shape[0]):
        part2 += "".join(display[r])
        part2 += "\n"

    return (part1, part2)


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 8))
    print(f"Part 1: {part1} | Part 2: {part2}")
