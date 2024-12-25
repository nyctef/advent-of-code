from util import get_input
import numpy as np


def rotate(degrees: int):
    theta = np.radians(degrees)
    return np.array([[np.cos(theta), -np.sin(theta)], [np.sin(theta), np.cos(theta)]])


def main():
    input = get_input(2016, 1)

    instructions = [(i[0], int(i[1:])) for i in input.strip().split(", ")]

    pos = np.array([0, 0])
    # let's say y+ is North
    dir = np.array([0, 1])

    # rotations are anticlockwise by convention
    left_turn = np.rint(rotate(90)).astype(int)
    right_turn = np.rint(rotate(-90)).astype(int)

    part2 = None
    visited = set()

    for turn, distance in instructions:
        dir = dir @ (left_turn if turn == "L" else right_turn)

        for _ in range(distance):
            pos += dir

            if tuple(pos) in visited and part2 is None:
                part2 = np.abs(pos).sum()
            else:
                visited.add(tuple(pos))

    print(f"Part 1: {np.abs(pos).sum()}")
    print(f"Part 2: {part2}")


if __name__ == "__main__":
    main()
