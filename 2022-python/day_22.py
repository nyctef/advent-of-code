from collections import defaultdict
from enum import Enum
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

# TODO: try using named enum instead
class Direction(Enum):
    right = 0
    down = 1
    left = 2
    up = 3


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
                MapEntry(3, 4, RIGHT, 5, RIGHT, 2, LEFT, 1, RIGHT),
                MapEntry(4, 6, DOWN, 5, DOWN, 3, LEFT, 1, UP),
                MapEntry(5, 6, RIGHT, 2, UP, 3, UP, 4, UP),
                MapEntry(6, 1, LEFT, 2, RIGHT, 5, LEFT, 4, LEFT),
            ]
        case "puzzle":
            return [
                MapEntry(1, 2, RIGHT, 3, DOWN, 4, RIGHT, 6, RIGHT),
                MapEntry(2, 5, LEFT, 3, LEFT, 1, LEFT, 6, UP),
                MapEntry(3, 2, UP, 5, DOWN, 4, DOWN, 1, UP),
                MapEntry(4, 5, RIGHT, 6, DOWN, 1, RIGHT, 3, RIGHT),
                MapEntry(5, 2, LEFT, 6, LEFT, 4, LEFT, 3, UP),
                MapEntry(6, 5, UP, 2, DOWN, 1, DOWN, 4, UP),
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
        return Point(0, side_length - old_row - 1)
    if old_direction == DOWN and new_direction == LEFT:
        return Point(old_column, side_length - 1)
    if old_direction == LEFT and new_direction == UP:
        return Point(side_length - 1, side_length - old_row - 1)
    if old_direction == UP and new_direction == RIGHT:
        return Point(old_column, 0)

    # left turns
    if old_direction == RIGHT and new_direction == UP:
        return Point(side_length - 1, old_row)
    if old_direction == UP and new_direction == LEFT:
        return Point(side_length - old_column - 1, side_length - 1)
    if old_direction == LEFT and new_direction == DOWN:
        return Point(0, old_row)
    if old_direction == DOWN and new_direction == RIGHT:
        return Point(side_length - old_column - 1, 0)

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
        print(
            f"currently on grid {current_grid.id} at {current_position} facing {current_dir}: next {instr=}"
        )
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
                        print(
                            f"switched sides from side {current_grid.id} {current_position} to {next_grid.id} {next_position} {current_dir=} {next_dir=}"
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
        f"final position on side {current_grid.id} {current_grid.original_coords=} {current_position=} {current_dir=}"
    )
    print(
        (1000 * (current_grid.original_coords.r + current_position.r + 1))
        + (4 * (current_grid.original_coords.c + current_position.c + 1))
        + current_dir
    )


def sanity_check_map(map: list[MapEntry]):
    incoming_counts = defaultdict(int)
    for page in map:
        incoming_counts[page.right_page] += 1
        incoming_counts[page.down_page] += 1
        incoming_counts[page.left_page] += 1
        incoming_counts[page.up_page] += 1
    assert len(set(incoming_counts.values())) == 1


def print_grid(grid: list[CubeSide]):
    for side in grid:
        print(f"------ cube side {side.id}")
        for line in side.chars:
            print(line)


def sanity_check_get_new_point():
    for old_r in range(4):
        for old_c in range(4):
            for old_dir in range(4):
                if old_dir == RIGHT and old_c != 3:
                    continue
                if old_dir == DOWN and old_r != 3:
                    continue
                if old_dir == LEFT and old_c != 0:
                    continue
                if old_dir == UP and old_r != 0:
                    continue
                for new_dir in range(4):
                    try:
                        new_r, new_c = get_point_on_new_face(
                            Point(old_r, old_c), old_dir, new_dir, 4
                        )
                        if new_dir == RIGHT:
                            assert new_c == 0
                        if new_dir == DOWN:
                            assert new_r == 0
                        if new_dir == LEFT:
                            assert new_c == 3
                        if new_dir == UP:
                            assert new_r == 3

                        # switching between the top and left axis
                        # or the bottom and right axis
                        # preserves one of the numbers
                        if (
                            (old_dir == DOWN and new_dir == LEFT)
                            or (old_dir == RIGHT and new_dir == UP)
                            or (old_dir == LEFT and new_dir == DOWN)
                            or (old_dir == UP and new_dir == RIGHT)
                        ):
                            assert new_r == old_c or new_c == old_r
                    except AssertionError as e:
                        raise Exception(locals()) from e


def main():
    input_name = "puzzle"
    input_file, side_width = read_input(input_name)
    grid, instructions = parse_input(input_file, side_width)
    map = get_map(input_name)
    sanity_check_map(map)
    sanity_check_get_new_point()
    ## print_grid(grid)
    pprint(map)
    # pprint(parsed)
    run_path(grid, instructions, map)


main()
