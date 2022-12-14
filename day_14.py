from collections import defaultdict
from itertools import chain
from pathlib import Path
from pprint import pprint
from typing import Dict, Tuple


input_file = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"""
input_file = Path("input/14-1.txt").read_text()


def parse_seg(s: str):
    x = s.split(",")
    return int(x[0]) + int(x[1]) * 1j


def round_(x: complex):
    return round(x.real) + round(x.imag) * 1j


lines = input_file.splitlines()
segments = [[parse_seg(y) for y in x.split(" -> ")] for x in lines]

pprint(segments)


area: Dict[complex, str] = defaultdict(lambda: ".")
area[500 + 0j] = "+"
for line in segments:
    for s in range(len(line) - 1):
        start = line[s]
        end = line[s + 1]
        x = start
        d = end - start
        d = round_(d / abs(d))
        while True:
            area[x] = "#"
            x += d
            if x == end:
                area[x] = "#"
                break

xs = [round(x.real) for x in area.keys()]
ys = [round(x.imag) for x in area.keys()]
print(f"{min(xs)=} {max(xs)=} {min(ys)=} {max(ys)=}")


def print_area(area_):
    for y in range(min(ys), max(ys) + 1):
        for x in range(min(xs), max(xs) + 1):
            print(area_[x + 1j * y], end="")
        print()


def try_move_down(area, loc):
    if loc.imag > max(ys):
        return ("falling", 0)
    for candidate in [loc + 1j, loc + (-1 + 1j), loc + (+1 + 1j)]:
        if area[candidate] == ".":
            return ("drop", candidate)
    return ("stopped", loc)


def model_sand_unit(area):
    loc = 500 + 0j
    while True:
        (moved, loc) = try_move_down(area, loc)
        if moved == "falling":
            raise Exception("done!")
        if moved != "drop":
            break
    area[loc] = "o"


count = 0
while True:
    count += 1
    try:
        model_sand_unit(area)
    except:
        print_area(area)
        print(f"{count-1=}")
        break
