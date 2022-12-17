from collections import defaultdict
from dataclasses import dataclass
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


class Chamber:
    rows: dict[int, list[str]] = defaultdict(lambda: [" "] * 7)
    next_rock_row = 3


def draw_chamber(chamber: Chamber, max_height: int):
    for i in reversed(range(max_height + 1)):
        r = "".join(chamber.rows[i])
        print(f"|{r}|")
    print("+-------+")


def read_input(name: str):
    match name:
        case "example":
            return """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""
        case "puzzle":
            return Path("input/17-1.txt").read_text()
        case other:
            raise Exception(other)


def write_shape_to_chamber(chamber: Chamber, shape: Shape, position: complex):
    for point in shape.points:
        line = round((point + position).imag)
        col = round((point + position).real)
        chamber.rows[line][col] = "#"


def main():
    input_file = read_input("example")
    jets = [1 if c == ">" else -1 for c in input_file]
    # print(jets)

    chamber = Chamber()
    next_shape_index = 0
    dropped_shapes_counter = 0
    while dropped_shapes_counter < 1:
        next_shape = shapes[next_shape_index]

        shape_position = 2 + chamber.next_rock_row * 1j

        # chamber.next_rock_row += 1

        write_shape_to_chamber(chamber, next_shape, shape_position)
        draw_chamber(chamber, 7)
        next_shape_index = (next_shape_index + 1) % len(shapes)
        dropped_shapes_counter += 1


if __name__ == "__main__":
    main()
