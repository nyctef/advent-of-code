from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().splitlines()

    triangles = np.loadtxt(input, dtype=int)

    test1 = triangles[:, 0] < triangles[:, 1] + triangles[:, 2]
    test2 = triangles[:, 1] < triangles[:, 2] + triangles[:, 0]
    test3 = triangles[:, 2] < triangles[:, 0] + triangles[:, 1]
    part1 = (test1 & test2 & test3).sum()

    triangles = np.vstack(
        [tris.T for tris in np.vsplit(triangles, triangles.shape[0] / 3)]
    )

    test1 = triangles[:, 0] < triangles[:, 1] + triangles[:, 2]
    test2 = triangles[:, 1] < triangles[:, 2] + triangles[:, 0]
    test3 = triangles[:, 2] < triangles[:, 0] + triangles[:, 1]
    part2 = (test1 & test2 & test3).sum()

    return (part1, part2)


def test_example_input():
    example = """
1  1  1
1  1  20
1  20 1
20 1  1
1  2  3
4  5  6
"""
    (part1, part2) = solve_for(example)

    assert part1 == 2
    assert part2 == 1


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 3))
    print(f"Part 1: {part1} | Part 2: {part2}")
