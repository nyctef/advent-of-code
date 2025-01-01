from collections import deque
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
    moves = set()
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

                if abs(a.x - b.x) <= 1 and abs(a.y - b.y) <= 1:
                    moves.add(a)
                    moves.add(b)

    node_map = {(node.x, node.y): node for node in nodes}
    start = node_map[(0, 0)]
    max_x = max(node.x for node in nodes)
    max_y = max(node.y for node in nodes)
    max_avail = max(node.size - node.used for node in nodes)
    target = node_map[(max_x, 0)]
    zero = [node for node in nodes if node.used == 0]
    wall = set((node.x, node.y) for node in nodes if node.used > max_avail)
    print(f"{zero=}")

    for y in range(max_y + 1):
        for x in range(max_x + 1):
            node = node_map.get((x, y))
            if node is None:
                print(" ", end="")
            elif node.used > max_avail:
                print("#", end="")
            elif node.used == 0:
                print("0", end="")
            elif node == start:
                print("S", end="")
            elif node == target:
                print("T", end="")
            elif node in moves:
                print("?", end="")
            else:
                print(".", end="")

        print()

    print(start, target)
    print(list(node for node in nodes if node.size - node.used >= target.used))

    part2 = "???"
    search = deque()
    seen = {}
    search.append((0, (zero[0].x, zero[0].y), (target.x, target.y)))
    while len(search):
        (turns, (zx, zy), (tx, ty)) = search.popleft()
        best_turns_here = seen.get((zx, zy, tx, ty))
        if best_turns_here is not None and best_turns_here <= turns:
            continue
        seen[(zx, zy, tx, ty)] = turns

        if (tx, ty) == (0, 0):
            part2 = turns
            break

        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nz = (zx + dx, zy + dy)
            if nz[0] > max_x or nz[0] < 0 or nz[1] > max_y or nz[1] < 0:
                continue
            if nz in wall:
                continue
            nt = (tx, ty)
            if nz == nt:
                # the zero and target are swapping
                nt = zx, zy
            search.append((turns + 1, nz, nt))

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
