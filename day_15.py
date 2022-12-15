from collections import namedtuple
from pathlib import Path
from pprint import pprint
import re
from typing import List, Tuple


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

# pprint(inputs)
# pprint(cleared_areas)
# pprint(beacons)

# inclusive range
Range = namedtuple("Range", "min, max")


def range_intersect(a: Range, b: Range):
    if a.max < b.min or b.max < a.min:
        return None
    return Range(min(a.min, b.min), max(a.max, b.max))


def find_covered_points_in_row(y):
    ranges: List[Range] = []
    # x_range = range(-3_000_000, 5_000_000)
    # y = 10
    for sensor, distance in cleared_areas:
        y_dist = round(abs(sensor.imag - y))
        remaining_dist = distance - y_dist
        if remaining_dist < 0:
            continue
        sensor_x = round(sensor.real)
        ranges.append(Range(sensor_x - remaining_dist, sensor_x + remaining_dist))

    # print(sorted(ranges))

    merged_ranges = []
    for r in sorted(ranges):
        to_remove = []
        intersected = False
        for i in range(len(merged_ranges)):
            intersected = range_intersect(r, merged_ranges[i])
            if intersected:
                to_remove.append(merged_ranges[i])
                merged_ranges.append(intersected)
        if not intersected:
            merged_ranges.append(r)
        for tr in to_remove:
            merged_ranges.remove(tr)
        # print(merged_ranges)

    result = 0
    for mr in merged_ranges:
        result += mr.max - mr.min + 1
    for b in beacons:
        for r in merged_ranges:
            if b.imag == y and b.real >= r.min and b.real <= r.max:
                # print(f"skipping beacon at {b=}")
                result -= 1
    return result


print(find_covered_points_in_row(2_000_000))
# pprint(mh_dist_abs(8 + 7j, 2 + 10j))
