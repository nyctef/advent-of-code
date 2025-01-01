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

    part2 = ""

    return (len(part1), part2)


@dataclass(order=True, frozen=True)
class Node:
    name: str
    size: int
    used: int


node_re = re.compile(r"([\/a-z-0-9]+)\s+(\d+)T\s+(\d+)T\s+(\d+)T")


def parse_node(input: str):
    if not (m := node_re.match(input)):
        raise Exception(f"couldn't parse line {input}")

    (name, size, used, avail) = (
        m.group(1),
        int(m.group(2)),
        int(m.group(3)),
        int(m.group(4)),
    )

    assert avail == size - used

    return Node(name, size, used)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 22))
    print(f"Part 1: {part1} | Part 2: {part2}")
