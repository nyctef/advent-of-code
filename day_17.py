from collections import defaultdict
from pathlib import Path
from pprint import pprint
from typing import NamedTuple


class Shape(NamedTuple):
    points: list[complex]

    @property
    def width(self):
        """
        eg the ---- shape with (0,0), (1,0), (2,0), (3,0) has width 4
        """
        return max(p.real for p in self.points) + 1

    @property
    def height(self):
        return max(p.imag for p in self.points) + 1


shapes: list[Shape] = [
    # ####
    Shape([0 + 0j, 1 + 0j, 2 + 0j, 3 + 0j]),
    # .#.
    # ###
    # .#.
    Shape([1 + 0j, 0 + 1j, 1 + 1j, 2 + 1j, 1 + 2j]),
    # ..#
    # ..#
    # ###
    Shape([2 + 0j, 2 + 1j, 0 + 2j, 1 + 2j, 2 + 2j]),
    # #
    # #
    # #
    # #
    Shape([0 + 0j, 0 + 1j, 0 + 2j, 0 + 3j]),
    # ##
    # ##
    Shape([0 + 0j, 0 + 1j, 1 + 0j, 1 + 1j]),
]

Chamber = dict[int, str]


def draw_chamber(chamber: Chamber, max_height: int):
    for i in reversed(range(max_height + 1)):
        print(f"|{chamber[i]}|")
    print("+-------+")


def read_input(name: str):
    match name:
        case "example":
            return """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""
        case "puzzle":
            return Path("input/17-1.txt").read_text()
        case other:
            raise Exception(other)


def main():
    input_file = read_input("example")
    jets = [1 if c == ">" else -1 for c in input_file]
    print(jets)

    chamber = defaultdict(lambda: " " * 7)
    draw_chamber(chamber, 7)


if __name__ == "__main__":
    main()
