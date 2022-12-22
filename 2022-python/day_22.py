from pathlib import Path
from pprint import pprint
import re
from typing import Tuple


def read_input(name: str):
    match name:
        case "example":
            return """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
"""
        case "puzzle":
            return Path("input/22-1.txt").read_text()
        case other:
            raise Exception(other)


def parse_input(inf: str):
    grid, instructions = inf.split("\n\n")
    grid = grid.splitlines()
    maxlen = max(len(g) for g in grid)
    grid = [g.ljust(maxlen) for g in grid]
    instructions = re.split(r"([LR])", instructions.strip())
    return (grid, instructions)


Point = Tuple[int, int]


def add(a: Point, b: Point):
    return (a[0] + b[0], a[1] + b[1])


def run_path(parsed):
    grid, instructions = parsed
    # right, down, left, up
    # remember row, column
    dirs: list[Point] = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    # initially facing right
    current_dir = 0

    def turn_right():
        nonlocal current_dir
        current_dir = (current_dir + 1) % len(dirs)

    def turn_left():
        nonlocal current_dir
        current_dir = (current_dir - 1) % len(dirs)

    def wrap(pos: Tuple[int, int]):
        r, c = pos
        if r < 0:
            r = len(grid) - 1
        if r >= len(grid):
            r = 0
        if c < 0:
            c = len(grid[0]) - 1
        if c >= len(grid[0]):
            c = 0
        return (r, c)

    def char_at(p: Point):
        return grid[p[0]][p[1]]

    def wrap_whitespace(p: Point, dir: Point):
        count = 0
        while char_at(p) == " ":
            count += 1
            if count >= 999_999:
                raise Exception(f"loop! {p=} {dir=}")

            p = wrap(add(p, dir))
        return p

    first_line = grid[0]
    initial_column = next(i for i, e in enumerate(first_line) if e != " ")
    print(f"{initial_column=}")

    # row, column (0-offset, but final result will need to be 1-offset so remember that)
    current_position = (0, initial_column)
    for instr in instructions:
        print(f"next {instr=} {current_position=}")
        if instr == "L":
            turn_left()
        elif instr == "R":
            turn_right()
        else:
            steps = int(instr)
            for _ in range(steps):
                next_position = add(current_position, dirs[current_dir])
                next_position = wrap(next_position)
                next_char = char_at(next_position)

                if next_char == " ":
                    next_position = wrap_whitespace(next_position, dirs[current_dir])
                    next_char = char_at(next_position)

                if next_char == ".":
                    current_position = next_position
                elif next_char == "#":
                    break
                elif next_char == " ":
                    raise Exception(next_position)
    print(
        (1000 * (current_position[0] + 1))
        + (4 * (current_position[1] + 1))
        + current_dir
    )


def main():
    input_file = read_input("example")
    parsed = parse_input(input_file)
    # pprint(parsed)
    run_path(parsed)


main()
