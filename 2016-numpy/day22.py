from dataclasses import dataclass
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()
    lines = lines[2:]

    nodes = [parse_node(line) for line in lines]

    part1 = set()
    for a in nodes:
        for b in nodes:
            if a == b:
                continue

            if a.used == 0:
                continue

            b_free = b.size - b.used
            if b_free >= a.used:
                pair = (a, b) if b > a else (b, a)
                # print(pair)
                part1.add(pair)

    node_map = {(node.x, node.y): node for node in nodes}
    start = node_map[(0, 0)]
    target_x = max(node.x for node in nodes)
    target = node_map[(target_x, 0)]

    print(start, target)
    print(list(node for node in nodes if node.size - node.used >= target.used))

    part2 = ""

    return (len(part1), part2)


@dataclass(order=True, frozen=True)
class Node:
    x: int
    y: int
    size: int
    used: int


node_re = re.compile(r"\/dev\/grid\/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T")


def parse_node(input: str):
    if not (m := node_re.match(input)):
        raise Exception(f"couldn't parse line {input}")

    (x, y, size, used, avail) = tuple(int(x) for x in m.groups())

    assert avail == size - used

    return Node(x, y, size, used)


def test_example_input():
    example = """
df
Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%
"""
    (part1, part2) = solve_for(example)

    assert part1 == 7
    assert part2 == 7


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 22))
    print(f"Part 1: {part1} | Part 2: {part2}")
