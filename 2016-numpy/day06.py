from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    grid = np.empty((len(lines), len(lines[0])), dtype="S1")
    for i, line in enumerate(lines):
        grid[i] = list(line)

    grid = grid.T

    # todo: understand some of the other approaches in
    # https://stackoverflow.com/questions/16330831/most-efficient-way-to-find-mode-in-numpy-array
    def mode_1d(arr: NDArray):
        (values, counts) = np.unique_counts(arr)
        return values[counts.argmax()]

    modes = np.apply_along_axis(mode_1d, axis=1, arr=grid)
    modes = np.strings.decode(modes)
    part1 = "".join(modes)

    def unmode_1d(arr: NDArray):
        (values, counts) = np.unique_counts(arr)
        return values[counts.argmin()]

    unmodes = np.apply_along_axis(unmode_1d, axis=1, arr=grid)
    unmodes = np.strings.decode(unmodes)
    part2 = "".join(unmodes)

    return (part1, part2)


def test_example_input():
    example = """
eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
"""
    (part1, part2) = solve_for(example)

    assert part1 == "easter"
    assert part2 == "advent"


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 6))
    print(f"Part 1: {part1} | Part 2: {part2}")
