from pathlib import Path
from pprint import pprint
import re


input_file = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"""
input_file = Path("input/15-1.txt").read_text()


def numbers(s: str):
    return [int(x) for x in re.findall(r"\-?\d+", s)]


def mh_dist_abs(a: complex, b: complex):
    return round(abs(a.real - b.real) + abs(a.imag - b.imag))


inputs = [numbers(l) for l in input_file.splitlines()]
inputs = [(sx + 1j * sy, bx + 1j * by) for [sx, sy, bx, by] in inputs]

cleared_areas = [(x[0], mh_dist_abs(x[0], x[1])) for x in inputs]

beacons = set((x[1] for x in inputs))

pprint(inputs)
pprint(cleared_areas)
pprint(beacons)

result = 0
y = 2_000_000
x_range = range(-3_000_000, 5_000_000)
# y = 9
# x_range = range(-10, 40)
for x in x_range:
    covered = False
    if complex(x, y) in beacons:
        continue
    for sensor, distance in cleared_areas:
        if mh_dist_abs(complex(x, y), sensor) <= distance:
            covered = True
            break
    if covered:
        result += 1

print(result)

# pprint(mh_dist_abs(8 + 7j, 2 + 10j))
