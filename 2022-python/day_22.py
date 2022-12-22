from pathlib import Path
from pprint import pprint
import re
from typing import NamedTuple, Tuple


def read_input(name: str):
    match name:
        case "example":
            return (
                """        ...#
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
""",
                4,
            )
        case "puzzle":
            return (Path("input/22-1.txt").read_text(), 50)
        case other:
            raise Exception(other)


RIGHT = 0
DOWN = 1
LEFT = 2
UP = 3


class MapEntry(NamedTuple):
    id: int
    right_page: int
    dir_after_right: int
    down_page: int
    dir_after_down: int
    left_page: int
    dir_after_left: int
    up_page: int
    dir_after_up: int


def get_map(name: str):
    match name:
        case "example":
            return [
                MapEntry(1, 6, LEFT, 4, DOWN, 3, DOWN, 2, DOWN),
                MapEntry(2, 3, RIGHT, 5, UP, 6, UP, 1, DOWN),
                MapEntry(3, 4, RIGHT, 5, RIGHT, 2, LEFT, 1, DOWN),
                MapEntry(4, 6, DOWN, 5, DOWN, 3, LEFT, 1, UP),
                MapEntry(5, 6, RIGHT, 2, LEFT, 3, UP, 4, UP),
                MapEntry(6, 1, LEFT, 2, RIGHT, 5, LEFT, 4, LEFT),
            ]
        case other:
            raise Exception(other)


class Point(NamedTuple):
    r: int
    c: int


class CubeSide(NamedTuple):
    id: int
    chars: list[str]
    side_width: int
    original_coords: Point


def parse_input(inf: str, side_width: int):
    grid_lines, instructions = inf.split("\n\n")
    grid_lines = grid_lines.splitlines()
    maxlen = max(len(g) for g in grid_lines)
    grid_lines = [g.ljust(maxlen) for g in grid_lines]
    instructions = re.split(r"([LR])", instructions.strip())

    grid: list[CubeSide] = []
    side_id = 1
    for big_row in range(0, len(grid_lines), side_width):
        for big_col in range(0, len(grid_lines[0]), side_width):
            if grid_lines[big_row][big_col] == " ":
                continue
            relevant_lines = grid_lines[big_row : big_row + side_width]
            relevant_substrs = [
                l[big_col : big_col + side_width] for l in relevant_lines
            ]

            grid.append(
                CubeSide(side_id, relevant_substrs, side_width, Point(big_row, big_col))
            )
            side_id += 1
    return (grid, instructions)


def get_point_on_new_face(
    old_point: Point, old_direction: int, new_direction: int, side_length: int
):
    if not (
        (old_point[0] in (0, side_length - 1)) or (old_point[1] in (0, side_length - 1))
    ):
        raise Exception(
            f"bad point {old_point=} {old_direction=} {new_direction=} {side_length=}"
        )

    old_row, old_column = old_point

    # straight lines
    if old_direction == DOWN and new_direction == DOWN:
        # appear at the top of the next face
        return Point(0, old_column)
    if old_direction == UP and new_direction == UP:
        # appear at the bottom of the next face
        return Point(side_length - 1, old_column)
    if old_direction == RIGHT and new_direction == RIGHT:
        # appear at the left (first column) of the next face
        return Point(old_row, 0)
    if old_direction == LEFT and new_direction == LEFT:
        # appear at the right (last column) of the next face
        return Point(old_row, side_length - 1)

    # right turns
    if old_direction == RIGHT and new_direction == DOWN:
        # starting from the rhs of one side, ending up on the top row of the next
        # closest corner is the bottom right of the original side which maps to the top left of the next
        return Point(0, side_length - old_row)
    if old_direction == DOWN and new_direction == LEFT:
        return Point(old_column, side_length - 1)
    if old_direction == LEFT and new_direction == UP:
        return Point(side_length - 1, side_length - old_row)
    if old_direction == UP and new_direction == RIGHT:
        return Point(old_column, 0)

    # left turns
    if old_direction == RIGHT and new_direction == UP:
        return Point(side_length - 1, old_row)
    if old_direction == UP and new_direction == LEFT:
        return Point(side_length - old_column, side_length - 1)
    if old_direction == LEFT and new_direction == DOWN:
        return Point(0, old_row)
    if old_direction == DOWN and new_direction == RIGHT:
        return Point(side_length - old_column, 0)

    # flips
    if (old_direction == RIGHT and new_direction == LEFT) or (
        old_direction == LEFT and new_direction == RIGHT
    ):
        return Point(side_length - old_row - 1, old_column)
    if (old_direction == DOWN and new_direction == UP) or (
        old_direction == UP and new_direction == DOWN
    ):
        return Point(old_row, side_length - old_column - 1)

    raise Exception(
        f"unhandled directions {old_point=} {old_direction=} {new_direction=} {side_length=}"
    )


def add(a: Point, b: Point):
    return Point(a[0] + b[0], a[1] + b[1])


def run_path(grid: list[CubeSide], instructions: list[str], map: list[MapEntry]):
    # right, down, left, up
    # remember row, column
    dirs: list[Point] = [Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0)]
    # initially facing right
    current_dir = 0

    def turn_right():
        nonlocal current_dir
        current_dir = (current_dir + 1) % len(dirs)

    def turn_left():
        nonlocal current_dir
        current_dir = (current_dir - 1) % len(dirs)

    current_grid = grid[0]

    def char_at(cs: CubeSide, p: Point):
        return cs.chars[p[0]][p[1]]

    # row, column (0-offset, but final result will need to be 1-offset so remember that)
    current_position = Point(0, 0)
    for instr in instructions:
        print(f"next {instr=} {current_position=} {current_grid.id=}")
        if instr == "L":
            turn_left()
        elif instr == "R":
            turn_right()
        else:
            steps = int(instr)
            for _ in range(steps):
                next_position = add(current_position, dirs[current_dir])
                next_grid = current_grid
                next_dir = current_dir
                if (
                    next_position.r < 0
                    or next_position.r >= current_grid.side_width
                    or next_position.c < 0
                    or next_position.c >= current_grid.side_width
                ):
                    mp = next(p for p in map if p.id == current_grid.id)
                    next_grid_id = [
                        mp.right_page,
                        mp.down_page,
                        mp.left_page,
                        mp.up_page,
                    ][current_dir]
                    next_dir = [
                        mp.dir_after_right,
                        mp.dir_after_down,
                        mp.dir_after_left,
                        mp.dir_after_up,
                    ][current_dir]
                    next_grid = next(g for g in grid if g.id == next_grid_id)
                    try:
                        next_position = get_point_on_new_face(
                            current_position,
                            current_dir,
                            next_dir,
                            current_grid.side_width,
                        )
                    except Exception as e:
                        raise Exception(
                            f"{current_position=} {next_position=} {current_dir=} {next_dir=} {current_grid.id=} {next_grid.id=}"
                        ) from e

                next_char = char_at(next_grid, next_position)

                if next_char == ".":
                    current_position = next_position
                    current_grid = next_grid
                    current_dir = next_dir
                elif next_char == "#":
                    break
                else:
                    raise Exception(next_position)
    print(
        (1000 * (current_position[0] + 1))
        + (4 * (current_position[1] + 1))
        + current_dir
    )


def main():
    input_name = "example"
    input_file, side_width = read_input(input_name)
    grid, instructions = parse_input(input_file, side_width)
    map = get_map(input_name)
    # pprint(parsed)
    run_path(grid, instructions, map)


main()
