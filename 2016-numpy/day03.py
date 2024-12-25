from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().splitlines()

    triangles = np.loadtxt(input, dtype=int)

    part1 = count_valid_triangles(triangles)

    # reshape(-1, 3, 3): leave first dimension unspecified, and group into 3x3 matrices
    # transpose(0, 2, 1): swap the last two dimensions (make the 3rd dimension the second and vice-versa)
    # reshape(-1, 3): leave first dimension unspecified, and group into 3-element arrays again
    triangles = triangles.reshape(-1, 3, 3).transpose(0, 2, 1).reshape(-1, 3)

    part2 = count_valid_triangles(triangles)

    return (part1, part2)


def count_valid_triangles(triangles):
    test1 = triangles[:, 0] < triangles[:, 1] + triangles[:, 2]
    test2 = triangles[:, 1] < triangles[:, 2] + triangles[:, 0]
    test3 = triangles[:, 2] < triangles[:, 0] + triangles[:, 1]
    return (test1 & test2 & test3).sum()


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
