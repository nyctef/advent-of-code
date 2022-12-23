from collections import defaultdict
from dataclasses import dataclass
from itertools import chain
from pathlib import Path
from pprint import pprint
from typing import NamedTuple


def read_input(name: str):
    match name:
        case "small-example":
            return """.....
..##.
..#..
.....
..##.
.....
"""
        case "big-example":
            return """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""
        case "puzzle":
            return Path("input/23-1.txt").read_text()

        case other:
            raise Exception(other)


def parse_input(input_file: str):
    lines = input_file.splitlines()
    points: list[complex] = []
    for r, row in enumerate(lines):
        for c, char in enumerate(row):
            if char == ".":
                continue
            elif char == "#":
                points.append(c + r * 1j)
            else:
                raise Exception(locals())
    return points


@dataclass
class Elf:
    current: complex
    proposed: complex


def run_puzzle(points: list[complex], num_rounds=1):
    elfs = [Elf(p, 0) for p in points]
    directions_to_move = [(0 - 1j), (0 + 1j), (-1 + 0j), (1 + 0j)]
    directions_to_check = [
        [0 - 1j, 1 - 1j, -1 - 1j],
        [0 + 1j, -1 + 1j, +1 + 1j],
        [-1 + 0j, -1 + 1j, -1 - 1j],
        [1 + 0j, 1 + 1j, 1 - 1j],
    ]
    eight_directions = [
        -1 - 1j,
        -1 - 0j,
        -1 + 1j,
        0 - 1j,
        0 + 1j,
        1 - 1j,
        1 + 0j,
        1 + 1j,
    ]
    assert set(chain.from_iterable(directions_to_check)) == set(eight_directions)
    round_num = 0
    while True:
        current_elf_locations = set(e.current for e in elfs)
        proposed: dict[complex, int] = defaultdict(int)

        starting_direction = round_num % 4
        four_directions = [(starting_direction + x) % 4 for x in range(0, 4)]

        def can_move_in_direction(my_pos: complex, direction: int):
            for direction_to_check in directions_to_check[direction]:
                if my_pos + direction_to_check in current_elf_locations:
                    return False
            return True

        def move_in_direction(my_pos: complex, direction: int):
            return my_pos + directions_to_move[direction]

        print(f"round {round_num}: {starting_direction=} {four_directions=}")
        any_elf_needs_to_move = False
        for elf in elfs:
            elf.proposed = elf.current
            nearby_elfs = [
                p
                for p in eight_directions
                if (elf.current + p) in current_elf_locations
            ]
            if nearby_elfs:
                any_elf_needs_to_move = True
            else:
                # this elf doesn't move since they have all the personal space required
                continue

            for direction in four_directions:
                if not can_move_in_direction(elf.current, direction):
                    continue
                else:
                    elf.proposed = move_in_direction(elf.current, direction)
                    proposed[elf.proposed] += 1
                    break
        if not any_elf_needs_to_move:
            break
        for elf in elfs:
            if elf.current != elf.proposed and proposed[elf.proposed] <= 1:
                elf.current = elf.proposed
            else:
                # conflict, can't move
                elf.proposed = elf.current
        # print([e.current for e in elfs])
        round_num += 1

    bounding_top = min(e.current.imag for e in elfs)
    bounding_left = min(e.current.real for e in elfs)
    bounding_bot = max(e.current.imag for e in elfs)
    bounding_right = max(e.current.real for e in elfs)
    bounding_width = bounding_right - bounding_left + 1
    bounding_height = bounding_bot - bounding_top + 1
    bounding_area = bounding_width * bounding_height
    free_space = bounding_area - len(elfs)
    pprint([x for x in locals().items() if x[0].startswith("bounding_")])
    print(free_space)
    print(f"round: {round_num + 1}")


def main(name: str):
    points = parse_input(read_input(name))
    run_puzzle(points)


if __name__ == "__main__":
    main("puzzle")
