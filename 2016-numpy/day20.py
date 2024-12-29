import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()
    ranges = []
    for line in lines:
        m = re.match(r"^(\d+)-(\d+)$", line)
        (start, end) = tuple(int(x) for x in m.groups())  # type: ignore
        ranges.append((start, end))

    ranges.sort()

    # print(ranges)

    max = 4294967295

    low = find_next_allowed(ranges, 0)

    part1 = low

    part2 = 0

    while True:

        high = find_last_allowed(ranges, low, max)
        # print(f"found allowed range from {low} to {high}")
        # assert high >= low
        part2 += high - low + 1
        if high == max:
            break
        low = find_next_allowed(ranges, high + 1)

    return (part1, part2)


def find_next_allowed(ranges, start):
    low = start
    too_low = True
    while too_low:
        too_low = False
        for start, end in ranges:
            # print(f"checking {low} against {(start, end)}")
            if start <= low and end > low:
                low = end + 1
                too_low = True
    return low


def find_last_allowed(ranges, start, max):
    try:
        return next(s - 1 for (s, e) in ranges if s > start)
    except StopIteration:
        return max


def test_example_input():
    example = """
5-8
0-2
4-7
"""
    (part1, part2) = solve_for(example)

    assert part1 == 3
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 20))
    print(f"Part 1: {part1} | Part 2: {part2}")
