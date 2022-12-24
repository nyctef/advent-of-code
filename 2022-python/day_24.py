from pathlib import Path
from pprint import pprint
from typing import NamedTuple


def read_input(name: str):
    match name:
        case "small":
            return """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
"""
        case "big":
            return """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""
        case "puzzle":
            return Path("input/24-1.txt").read_text()
        case other:
            raise Exception(other)


class Point(NamedTuple):
    r: int
    c: int


class Blizzard(NamedTuple):
    loc: Point
    dir: Point


class Field(NamedTuple):
    height: int
    width: int
    start: Point
    end: Point
    blizz: list[Blizzard]


def add(a: Point, b: Point):
    return Point(a.r + b.r, a.c + b.c)


directions = {
    ">": Point(0, 1),
    "v": Point(1, 0),
    "<": Point(0, -1),
    "^": Point(-1, 0),
}
r_directions = {v: k for k, v in directions.items()}


def parse_input(input_file: str):
    start: Point | None = None
    end: Point | None = None
    blizz: list[Blizzard] = []
    lines = input_file.splitlines()
    field_height = len(lines) - 2
    field_width = len(lines[0]) - 2
    for r, line in enumerate(lines):
        for c, char in enumerate(line):
            field_r = r - 1
            field_c = c - 1
            if r == 0 and char == ".":
                start = Point(field_r, field_c)
            elif r == len(lines) - 1 and char == ".":
                end = Point(field_r, field_c)
            elif char in (">", "v", "<", "^"):
                blizz.append(Blizzard(Point(field_r, field_c), directions[char]))

    assert start is not None
    assert end is not None
    return Field(field_height, field_width, start, end, blizz)


def print_field(f: Field):
    blizz_by_point = {b.loc: b for b in f.blizz}
    print("-" * f.width)
    for r in range(f.height):
        for c in range(f.width):
            b = blizz_by_point.get(Point(r, c), None)
            if b is not None:
                print(r_directions[b.dir], end="")
            else:
                print(" ", end="")
        print()
    print("-" * f.width)


def main(name: str):
    input_file = read_input(name)
    field = parse_input(input_file)
    pprint(field)
    print_field(field)


if __name__ == "__main__":
    main("small")
