from collections import defaultdict
from itertools import chain
from pprint import pprint
from typing import Dict, Tuple


input_file = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"""


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
                break

xs = [round(x.real) for x in area.keys()]
ys = [round(x.imag) for x in area.keys()]
print(f"{min(xs)=} {max(xs)=} {min(ys)=} {max(ys)=}")


print(area)

for y in range(min(ys), max(ys) + 1):
    for x in range(min(xs), max(xs) + 1):
        print(area[x + 1j * y], end="")
    print()
