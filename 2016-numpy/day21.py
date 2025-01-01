from util import get_input
import numpy as np
import re
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    password = np.array(list("abcdefgh"))

    for line in lines:
        if m := re.match(r"swap position (\d+) with position (\d+)", line):
            (l, r) = tuple(int(x) for x in m.groups())
            password[[l, r]] = password[[r, l]]
        elif m := re.match(r"swap letter (\w+) with letter (\w+)", line):
            (l, r) = m.groups()
            positions = np.nonzero(np.logical_or(password == l, password == r))[0]
            password[positions] = password[positions[::-1]]
        elif m := re.match(r"rotate right (\d+) steps?", line):
            r = int(m.group(1))
            password = np.roll(password, r)
        elif m := re.match(r"rotate left (\d+) steps?", line):
            r = int(m.group(1))
            password = np.roll(password, -r)
        elif m := re.match(r"rotate based on position of letter (\w+)", line):
            r = m.group(1)
            pos = np.nonzero(password == r)[0]
            if pos >= 4:
                pos += 1
            password = np.roll(password, pos + 1)
        elif m := re.match(r"reverse positions (\d+) through (\d+)", line):
            (l, r) = tuple(int(x) for x in m.groups())
            password[l : r + 1] = password[r : l - 1 if l else None : -1]
        elif m := re.match(r"move position (\d+) to position (\d+)", line):
            (l, r) = tuple(int(x) for x in m.groups())
            x = password[l]
            password = np.delete(password, [l])
            password = np.insert(password, [r], x)
        else:
            print(f"unrecognised instruction {line}")

    part1 = "".join(password)
    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 21))
    print(f"Part 1: {part1} | Part 2: {part2}")
