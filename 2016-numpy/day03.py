from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().splitlines()

    triangles = [np.array([int(x) for x in line.split()]) for line in input]

    part1 = 0
    for tri in triangles:
        pairs = np.array(np.meshgrid(tri, tri)).T.reshape(-1, 2)
        pairs = pairs[pairs[:, 0] < pairs[:, 1]]
        leftover = np.array([np.setdiff1d(tri, pair) for pair in pairs])
        print(pairs, leftover)
        if leftover.size == 0:
            # duplicate values means there isn't a third value left over
            part1 += 1
            continue
        valid = True
        for i in range(len(pairs)):
            if not pairs[i].sum() > leftover[i]:
                valid = False
                break
        if valid:
            part1 += 1

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 3))
    print(f"Part 1: {part1} | Part 2: {part2}")
