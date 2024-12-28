from collections import deque
from dataclasses import dataclass
import hashlib
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


def md5(input: str):
    return hashlib.md5(input.encode()).hexdigest()


def doors(input: str):
    hash = md5(input)
    open = ["b", "c", "d", "e", "f"]
    dirs = [Dir(0, -1), Dir(0, 1), Dir(-1, 0), Dir(1, 0)]
    dir_chars = ["U", "D", "L", "R"]
    result = []
    for i in range(4):
        if hash[i] in open:
            result.append((dir_chars[i], dirs[i]))
    return result


def solve_for(input: str):
    passcode = input.strip()

    search = deque()
    search.appendleft((passcode, Point(0, 0)))
    part1 = "????????????????????????????????"
    part2 = ""
    while len(search):
        code, pos = search.popleft()
        if pos == Point(3, 3):
            path = code[len(passcode) :]
            if len(path) < len(part1):
                part1 = path
            if len(path) > len(part2):
                part2 = path
            continue

        # print(f"{code=} {pos=} {doors(code)=}")
        for char, dir in doors(code):
            target_pos = pos + dir
            if (
                target_pos.x >= 0
                and target_pos.y >= 0
                and target_pos.x <= 3
                and target_pos.y <= 3
            ):
                search.append((code + char, target_pos))

    return (part1, len(part2))


def test_example_input():

    assert solve_for("ihgpwlah")[0] == "DDRRRD"
    assert solve_for("hijkl")[0] == "????????????????????????????????"


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 17))
    print(f"Part 1: {part1} | Part 2: {part2}")
