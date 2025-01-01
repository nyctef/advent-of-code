from collections import deque
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()
    nums = {}
    tiles = {}
    for r in range(len(lines)):
        for c in range(len(lines[0])):
            char = lines[r][c]
            tiles[(r, c)] = char
            if str.isdigit(char):
                nums[int(char)] = (r, c)
    dists = {}

    for num, num_pos in nums.items():
        search = deque()
        seen = {}
        search.append((0, num_pos))
        while len(search):
            (steps, pos) = search.popleft()
            best = seen.get(pos)
            if best is not None and best <= steps:
                continue
            seen[pos] = steps

            char = tiles[pos]
            # print(f"{num=} {steps=} {pos=} {char=}")
            if str.isdigit(char):
                dists[(num, int(char))] = steps

            for dir in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                np = (pos[0] + dir[0], pos[1] + dir[1])
                if tiles[np] != "#":
                    search.append((steps + 1, np))

    for (x, y), dist in dists.items():
        assert (dists[(y, x)]) == dist

    part1 = 9999999999999999
    search = deque()
    seen = {}
    search.append((0, [0]))
    while len(search):
        (steps, collected) = search.popleft()
        # sorted = list(collected)
        # sorted.sort()
        # key = (tuple(sorted), collected[-1])
        # best = seen.get(key)
        # if best is not None and best <= steps:
        #     continue
        # seen[key] = steps

        remaining = set(nums.keys()) - set(collected)

        if not len(remaining):
            print(f"{steps=} {collected=}")
            if steps < part1:
                part1 = steps
            continue

        prev_collected = collected[-1]
        for r in remaining:
            new_collected = list(collected)
            new_collected.append(r)
            new_steps = steps + dists[(prev_collected, r)]
            search.append((new_steps, new_collected))

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """
###########
#0.1.....2#
#.#######.#
#4.......3#
###########
"""
    (part1, part2) = solve_for(example)

    assert part1 == 14
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 24))
    print(f"Part 1: {part1} | Part 2: {part2}")
