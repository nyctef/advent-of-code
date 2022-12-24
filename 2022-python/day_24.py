import heapq
import math
from pathlib import Path
from pprint import pformat, pprint
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

    def after_time(self, minutes_passed: int, rows: int, columns: int):
        new_loc = (self.loc + (self.dir * minutes_passed)).mod(rows, columns)
        # print(f"{self.loc=} {self.dir=} {minutes_passed=} {rows=} {columns=}")
        return self._replace(loc=new_loc)


class Field(NamedTuple):
    height: int
    width: int
    start: Point
    end: Point
    blizz: list[Blizzard]

    def after_time(self, minutes_passed: int):
        updated_blizz = [
            b.after_time(minutes_passed, self.height, self.width) for b in self.blizz
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
    distance: int
    position: Point
    current_min: int
    prev: "SearchStep | None"

    def __str__(self) -> str:
        return f"min={self.current_min} r={self.position.r} c={self.position.c}"


def print_path(s: SearchStep | None):
    if s is not None:
        print(f"At min {s.current_min}: r={s.position.r} c={s.position.c}")
        print_path(s.prev)
    else:
        print("end.")


def nothing(*args):
    pass


log = nothing


def search_path(
    field: Field,
    starting_minute: int,
    starting_position: Point,
    goal: Point,
    blizz_cache: Dict[int, Set[Point]] = {},
):
    # cache from t cycle to field
    field_cycle_time = math.lcm(field.width, field.height)

    def get_occupied_points_at(t: int):
        r = blizz_cache.get(t, None)
        if r is None:
            r = set(b.loc for b in field.after_time(t).blizz)
            blizz_cache[t] = r
        return r

    seen_steps: Dict[SearchStep, int] = {}
    best_score = 9999999999999999
    best_path = None

    q: List[SearchStep] = [
        SearchStep(
            starting_position.mdist(goal), starting_position, starting_minute, None
        )
    ]
    count = 0
    state_skips = 0
    give_up_skips = 0

    def print_progress():
        print(
            f"{count=} {len(q)=} best={best_score} min={n.current_min} pos={n.position} dedupes={state_skips} best_skips={give_up_skips} seen={len(seen_steps)}"
        )

    while q:
        n = heapq.heappop(q)
        count += 1
        if (count % 100_000) == 0:
            print_progress()
        log()
        log(f" considering state {n}")
        remaining_distance = n.position.mdist(goal)
        dupe_of = SearchStep(
            remaining_distance, n.position, n.current_min % field_cycle_time, None
        )
        seen_at_time = seen_steps.get(dupe_of, None)
        if seen_at_time is not None and seen_at_time <= n.current_min:
            # auto-skip if we've considered this state before, and we considered it at an earlier time
            # we can't auto-skip if we've been in this cycle at a later minute, since that means
            # this round is actually an improvement
            log(
                f">>> rejecting {n}: already seen {dupe_of=} at earlier time {seen_at_time}"
            )
            state_skips += 1
            continue
        seen_steps[dupe_of] = n.current_min

        if n.position == goal:
            # complete!
            if n.current_min < best_score:
                # with a new PB!
                best_score = n.current_min
                best_path = n
            # log(f"found a path! {n}")
            continue

        if n.current_min + remaining_distance >= best_score:
            # skip if we can't possibly make it to the goal better than our PB
            log(f">>> rejecting {n}: {best_score=} {remaining_distance=}")
            give_up_skips += 1
            continue

        next_occupied_points = get_occupied_points_at(n.current_min + 1)
        if n.position not in next_occupied_points:
            # or we could just wait
            heapq.heappush(
                q, SearchStep(remaining_distance, n.position, n.current_min + 1, n)
            )

        log(f"min={n.current_min} r={n.position.r} c={n.position.c}")
        if n.current_min == 16 and n.position == Point(2, 5):
            log("****************")
        if n.current_min == 17 and n.position == Point(3, 5):
            log("################")
        directions = n.position.dir4()
        for candidate in directions:
            n2d = candidate.mdist(goal)
            n2 = SearchStep(n2d, candidate, n.current_min + 1, n)
            if candidate == goal:
                heapq.heappush(q, n2)
            elif candidate.r < 0 or candidate.r >= field.height:
                log(f"rejecting {candidate=} {field.width=} {field.height=}")
                pass
            elif candidate.c < 0 or candidate.c >= field.width:
                log(f"rejecting {candidate=} {field.width=} {field.height=}")
                pass
            elif candidate in next_occupied_points:
                log(f"rejecting {n2} due to being in a blizz")
            else:
                log(f"queuing up {n2}")
                heapq.heappush(q, n2)
        # log(f"{q=}")
        # if count > 10:
        #     break
    print_progress()
    print(f"{best_score=} {count=}")
    # print_path(best_path)
    return best_score


def main(name: str):
    input_file = read_input(name)
    field = parse_input(input_file)
    blizz_cache: Dict[int, Set[Point]] = {}
    time1 = search_path(field, 0, field.start, field.end, blizz_cache)
    time2 = search_path(field, time1, field.end, field.start, blizz_cache)
    time3 = search_path(field, time2, field.start, field.end, blizz_cache)
    print(f"{time1=} {time2=} {time3=}")
    # print_field(field)
    # pprint(field)
    # for t in range(19):
    #     print(t)
    #     print_field(field.after_time(t))


"""
    for b in field.blizz:
        try:
            b2 = b.after_time(0, field.height, field.width)
            assert b2 == b
        except AssertionError as e:
            raise Exception(f"{b=} {b2=} {field.width=} {field.height=}")
"""


if __name__ == "__main__":
    main("puzzle")
