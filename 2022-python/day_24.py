import math
from pathlib import Path
from pprint import pprint
from typing import Dict, List, NamedTuple, Set, Tuple


def read_input(name: str):
    match name:
        case "small":
            return """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
"""
        case "big":
            return """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""
        case "puzzle":
            return Path("input/24-1.txt").read_text()
        case other:
            raise Exception(other)


class Point(NamedTuple):
    r: int
    c: int

    def __add__(self, other):
        return Point(self.r + other.r, self.c + other.c)

    def __mul__(self, other):
        return Point(self.r * other, self.c * other)

    def mod(self, rmod, cmod):
        """Wrap the point within some bounding box"""
        return Point(self.r % rmod, self.c % cmod)

    def mdist(self, other: "Point"):
        """Manhattan distance"""
        return abs(self.r - other.r) + abs(self.c - other.c)

    def dir4(self):
        return [
            self + Point(0, -1),
            self + Point(-1, 0),
            self + Point(0, 1),
            self + Point(1, 0),
        ]


class Blizzard(NamedTuple):
    loc: Point
    dir: Point

    def after_time(self, minutes_passed: int, width: int, height: int):
        new_loc = (self.loc + (self.dir * minutes_passed)).mod(width, height)
        return self._replace(loc=new_loc)


class Field(NamedTuple):
    height: int
    width: int
    start: Point
    end: Point
    blizz: list[Blizzard]

    def after_time(self, minutes_passed: int):
        updated_blizz = [
            b.after_time(minutes_passed, self.width, self.height) for b in self.blizz
        ]
        return self._replace(blizz=updated_blizz)


directions = {
    ">": Point(0, 1),
    "v": Point(1, 0),
    "<": Point(0, -1),
    "^": Point(-1, 0),
}
r_directions = {v: k for k, v in directions.items()}


def parse_input(input_file: str):
    start: Point | None = None
    end: Point | None = None
    blizz: list[Blizzard] = []
    lines = input_file.splitlines()
    field_height = len(lines) - 2
    field_width = len(lines[0]) - 2
    for r, line in enumerate(lines):
        for c, char in enumerate(line):
            field_r = r - 1
            field_c = c - 1
            if r == 0 and char == ".":
                start = Point(field_r, field_c)
            elif r == len(lines) - 1 and char == ".":
                end = Point(field_r, field_c)
            elif char in (">", "v", "<", "^"):
                blizz.append(Blizzard(Point(field_r, field_c), directions[char]))

    assert start is not None
    assert end is not None
    return Field(field_height, field_width, start, end, blizz)


def print_field(f: Field):
    blizz_by_point = {b.loc: b for b in f.blizz}
    print("-" * f.width)
    for r in range(f.height):
        for c in range(f.width):
            b = blizz_by_point.get(Point(r, c), None)
            if b is not None:
                print(r_directions[b.dir], end="")
            else:
                print(" ", end="")
        print()
    print("-" * f.width)


class SearchStep(NamedTuple):
    position: Point
    current_min: int


def search_path(field: Field):
    # cache from t cycle to field
    blizz_cache: Dict[int, Set[Point]] = {}
    field_cycle_time = math.lcm(field.width, field.height)

    def get_occupied_points_at(t: int):
        r = blizz_cache.get(t, None)
        if r is None:
            r = set(b.loc for b in field.after_time(t).blizz)
            blizz_cache[t] = r
        return r

    seen_steps: Set[SearchStep] = set()
    best_score = 9999999999999999

    q: List[SearchStep] = [SearchStep(field.start, 0)]
    count = 0
    while q:
        n = q.pop()
        count += 1
        if (count % 10_000) == 0:
            print(f"{count=} {len(q)=} {best_score=} {n.current_min=} {n.position=}")
        if n in seen_steps:
            # auto-skip if we've considered this state before
            continue
        seen_steps.add(n)

        if n.position == field.end:
            # complete!
            if n.current_min < best_score:
                # with a new PB!
                best_score = n.current_min
            continue

        if n.current_min + n.position.mdist(field.end) >= best_score:
            # skip if we can't possibly make it to the goal better than our PB
            continue

        # or we could just wait
        q.append(SearchStep(n.position, n.current_min + 1))

        next_occupied_points = get_occupied_points_at(n.current_min + 1)
        for candidate in n.position.dir4():
            # print(f"{candidate=}")
            n2 = SearchStep(candidate, n.current_min + 1)
            if candidate == field.end:
                q.append(n2)
            elif candidate.r < 0 or candidate.r >= field.height:
                continue
            elif candidate.c < 0 or candidate.c >= field.width:
                continue
            elif candidate not in next_occupied_points:
                q.append(n2)
        # print(f"{q=}")
        # if count > 10:
        #     break
    print(f"{best_score=}")
    return best_score


def main(name: str):
    input_file = read_input(name)
    field = parse_input(input_file)
    time = search_path(field)


if __name__ == "__main__":
    main("big")
