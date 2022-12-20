from collections import defaultdict
from copy import deepcopy
from dataclasses import dataclass, field
from pathlib import Path
from pprint import pprint
from typing import NamedTuple
import sys

sys.stdout.reconfigure(encoding="utf-8")  # type: ignore


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
    # Shape([2 + 0j, 2 + 1j, 0 + 2j, 1 + 2j, 2 + 2j]),
    # ###
    # ..#
    # ..#
    Shape([0 + 0j, 1 + 0j, 2 + 0j, 2 + 1j, 2 + 2j]),
    # #
    # #
    # #
    # #
    Shape([0 + 0j, 0 + 1j, 0 + 2j, 0 + 3j]),
    # ##
    # ##
    Shape([0 + 0j, 0 + 1j, 1 + 0j, 1 + 1j]),
]


@dataclass
class Chamber:
    rows: dict[int, list[str]] = field(
        default_factory=lambda: defaultdict(lambda: [" "] * 7)
    )
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


def write_shape_to_chamber(
    chamber: Chamber, shape: Shape, position: complex, char: str
):
    # print(f"writing shape to chamber at {position=}")
    for point in shape.points:
        line = round((point + position).imag)
        col = round((point + position).real)
        # print(f"setting chamber {line=} {col=}")
        chamber.rows[line][col] = char


def try_move(shape: Shape, pos: complex, chamber: Chamber):
    for p in shape.points:
        translated_pos = p + pos
        if translated_pos.real < 0:
            # print("hit left side")
            return None
        if translated_pos.real >= 7:
            # print("hit right side")
            return None
        if translated_pos.imag < 0:
            # print("hit bottom")
            return None
        if chamber.rows[round(translated_pos.imag)][round(translated_pos.real)] != " ":
            # print("hit another shape")
            return None
    return pos


def draw_chamber_with_extra_shape(c: Chamber, shape: Shape, pos: complex):
    chamber = deepcopy(c)
    print(f"{id(c)=} {id(c.rows[0])=} {id(chamber)=} {id(chamber.rows[0])=}")
    write_shape_to_chamber(chamber, shape, pos, "@")
    draw_chamber(chamber, chamber.next_rock_row)


def simulate(jets: list[int], limit: int):
    chamber = Chamber()
    next_shape_index = 0
    dropped_shapes_counter = 0
    jet_index = 0
    while dropped_shapes_counter < limit:
        next_shape = shapes[next_shape_index]

        pos = 2 + chamber.next_rock_row * 1j

        while True:
            # print(f"jet {jet_index} is {jets[jet_index]=}")
            pos_from_jet = pos + jets[jet_index]
            jet_index = (jet_index + 1) % len(jets)
            # print(f"trying to apply jet {pos=} {pos_from_jet=}")
            jet_pos = try_move(next_shape, pos_from_jet, chamber)
            jet_pos = jet_pos if jet_pos is not None else pos

            # draw_chamber_with_extra_shape(chamber, next_shape, jet_pos)
            # print(f"trying to apply gravity {jet_pos=} {jet_pos - 1j=}")
            down_pos = try_move(next_shape, jet_pos - 1j, chamber)
            if down_pos is None:
                # hit something after trying to move down - shape comes to a rest
                # print("and stopped")
                pos = jet_pos
                break
            else:
                # draw_chamber_with_extra_shape(chamber, next_shape, down_pos)
                # otherwise loop
                pos = down_pos

        write_shape_to_chamber(chamber, next_shape, pos, "#")
        # draw_chamber(chamber, chamber.next_rock_row)

        while chamber.rows[chamber.next_rock_row - 3] != [" "] * 7:
            # print("bumping up next rock row")
            chamber.next_rock_row += 1

        # print(
        #     f"shape {next_shape_index=} landed at {pos=} so checking {round(pos.imag -  next_shape.height + 1)=}"
        # )
        floor = round(pos.imag - next_shape.height + 1)
        if chamber.rows[floor] == (["#"] * 7):
            print(
                f"found new floor at {dropped_shapes_counter=} {floor=} {next_shape_index=} {jet_index=}"
            )

        # print(chamber.next_rock_row - 3)

        next_shape_index = (next_shape_index + 1) % len(shapes)
        dropped_shapes_counter += 1

        if dropped_shapes_counter % 50_000 == 0:
            print(dropped_shapes_counter)
        if (jet_index == 0) and (next_shape_index == 0):
            print(dropped_shapes_counter)
            if chamber.rows[chamber.next_rock_row - 4] == (["#"] * 7):
                raise Exception(f"found a loop at {dropped_shapes_counter}!!!")
    print(chamber.next_rock_row - 3)
    print(chamber.rows[chamber.next_rock_row - 4])
    return chamber


def main():
    # input_file, limit = (read_input("example"), 10)
    input_file, limit = (read_input("puzzle"), 750)
    # input_file, limit = (read_input("puzzle"), 10)
    jets = [1 if c == ">" else -1 for c in input_file]

    # print(len(jets * 5))
    chamber = simulate(jets, limit)
    # draw_chamber(chamber, chamber.next_rock_row)

    # running_total = 0
    # for (i, jet) in enumerate(jets):
    #     running_total += jet
    #     print(f"{i}\t{running_total}")


if __name__ == "__main__":
    main()
