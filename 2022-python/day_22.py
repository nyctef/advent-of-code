from pathlib import Path
from pprint import pprint
import re
from types import SimpleNamespace as SN


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
    grid, directions = inf.split("\n\n")
    grid = grid.splitlines()
    maxlen = max(len(g) for g in grid)
    grid = [g.ljust(maxlen) for g in grid]
    directions = re.split(r"([LR])", directions.strip())
    return SN(grid=grid, directions=directions)


def main():
    input_file = read_input("example")
    parsed = parse_input(input_file)
    pprint(parsed)


main()
