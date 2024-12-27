from collections import deque
from dataclasses import dataclass
from util import get_input
import numpy as np
from numpy.typing import NDArray


@dataclass(frozen=True)
class Point:
    x: int
    y: int

    def __add__(self, other):
        if isinstance(other, Dir):
            return Point(self.x + other.x, self.y + other.y)

        return NotImplemented


@dataclass(frozen=True)
class Dir:
    x: int
    y: int

    @classmethod
    def four(cls):
        return [Dir(1, 0), Dir(0, -1), Dir(-1, 0), Dir(0, 1)]


def solve_for(input: str, target: Point):
    fav_num = int(input)

    start = Point(1, 1)

    def is_wall(p: Point):
        if p.x < 0 or p.y < 0:
            return True
        magic = p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y + fav_num
        return magic.bit_count() & 1 == 1

    search = deque()
    search.appendleft((0, start))
    seen = set()
    while len(search):
        (steps, pos) = search.popleft()
        if pos == target:
            part1 = steps
            break

        if pos in seen:
            continue
        seen.add(pos)

        for dir in Dir.four():
            next_pos = pos + dir
            if not is_wall(next_pos):
                search.append((steps + 1, next_pos))

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """10"""
    (part1, part2) = solve_for(example, Point(7, 4))

    assert part1 == 11
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 13), Point(31, 39))
    print(f"Part 1: {part1} | Part 2: {part2}")
