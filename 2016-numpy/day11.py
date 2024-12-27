from dataclasses import astuple, dataclass
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray
import heapq
from copy import deepcopy
import itertools


class MaxHeapObj(object):
    """
    based on https://stackoverflow.com/a/40455775/895407
    """

    def __init__(self, val):
        self.val = val

    def __lt__(self, other):
        return self.val > other.val

    def __eq__(self, other):
        return self.val == other.val

    def __str__(self):
        return str(self.val)


class MaxHeap:
    """
    based on https://stackoverflow.com/a/40455775/895407
    """

    h = []

    def __init__(self):
        h = []

    def push(self, val):
        heapq.heappush(self.h, MaxHeapObj(val))

    def pop(self):
        return heapq.heappop(self.h).val

    def __len__(self):
        return len(self.h)


class MinHeap:
    """
    based on https://stackoverflow.com/a/40455775/895407
    """

    h = []

    def __init__(self):
        h = []

    def push(self, val):
        heapq.heappush(self.h, val)

    def pop(self):
        return heapq.heappop(self.h)

    def __len__(self):
        return len(self.h)


def score(floors: list):
    # TODO: does the scoring function matter significantly here?
    return 3 * len(floors[3]) + len(floors[2])


def is_done(floors: list):
    return (
        not len(floors[0])
        and not len(floors[1])
        and not len(floors[2])
        and len(floors[3])
    )


def is_valid(floors):
    for floor in floors:
        for chip in (i for i in floor if i.startswith("mc")):
            element = chip[3:]
            if f"gen {element}" in floor:
                continue
            if any(i for i in floor if i.startswith("gen")):
                # some other generator on this floor is going to fry this chip
                return False
    return True


@dataclass
class State:
    turns: int
    elevator: int
    floors: list

    def __lt__(self, other):
        if self.turns != other.turns:
            return self.turns < other.turns
        # return score(self.floors) > score(other.floors)
        return False


def next_states(state: State):
    (turns, elevator, floors) = astuple(state)

    result = []
    # from each point on floors[elevator], we can take 1-2 items up or down
    for item in floors[elevator]:
        if elevator < len(floors) - 1:
            next_state_up = deepcopy(floors)
            next_state_up[elevator].remove(item)
            next_state_up[elevator + 1].append(item)
            result.append(State(turns + 1, elevator + 1, next_state_up))
        if elevator > 0:
            next_state_down = deepcopy(floors)
            next_state_down[elevator].remove(item)
            next_state_down[elevator - 1].append(item)
            result.append(State(turns + 1, elevator - 1, next_state_down))

    for item1, item2 in itertools.combinations(floors[elevator], 2):
        if elevator < len(floors) - 1:
            next_state_up = deepcopy(floors)
            next_state_up[elevator].remove(item1)
            next_state_up[elevator].remove(item2)
            next_state_up[elevator + 1].append(item1)
            next_state_up[elevator + 1].append(item2)
            result.append(State(turns + 1, elevator + 1, next_state_up))
        if elevator > 0:
            next_state_down = deepcopy(floors)
            next_state_down[elevator].remove(item1)
            next_state_down[elevator].remove(item2)
            next_state_down[elevator - 1].append(item1)
            next_state_down[elevator - 1].append(item2)
            result.append(State(turns + 1, elevator - 1, next_state_down))

    result = [State(s.turns, s.elevator, normalize_floors(s.floors)) for s in result]
    return [r for r in result if is_valid(r.floors)]


def solve_for(input: str):
    lines = input.strip().splitlines()
    floors = []
    for line in lines:
        floor = []
        for mc in re.finditer(r"(\w+)-compatible microchip", line):
            floor.append(f"mc {mc.group(1)}")
        for gen in re.finditer(r"(\w+) generator", line):
            floor.append(f"gen {gen.group(1)}")
        floors.append(floor)

    print(floors)

    # num turns, score for some kind of heuristic, current floor, floor state
    state = State(0, 0, floors)

    fewest_turns = 99999999999
    search = MinHeap()
    search.push(state)
    seen = {}
    counter = 1
    while len(search):
        current = search.pop()

        if counter % 10_000 == 0:
            print(
                f"  {len(search)=} {current.turns=} {score(current.floors)=} {fewest_turns=} {len(seen)=}"
            )
        counter += 1

        if current.turns >= fewest_turns:
            continue
        if is_done(current.floors):
            fewest_turns = current.turns
            continue

        seen_key = str(current.floors) + " " + str(current.elevator)
        best_at_floors = seen.get(seen_key)
        if best_at_floors is None or best_at_floors > current.turns:
            seen[seen_key] = current.turns
        else:
            continue

        for next in next_states(current):
            search.push(next)

    part1 = fewest_turns
    part2 = ""

    return (part1, part2)


def test_floors_valid():
    assert is_valid([["mc A"]]) == True
    assert is_valid([["gen A"]]) == True
    assert is_valid([["mc A", "gen A"]]) == True
    assert is_valid([["mc A", "gen A", "gen B"]]) == True
    assert is_valid([["mc A", "gen B"]]) == False


def normalize_floors(floors: list):
    name_counter = 1
    renames = {}
    result = []
    for floor in floors:
        new_floor = []
        for item in floor:
            (typ, name) = item.split(" ")
            if name in renames:
                name = renames[name]
            else:
                renames[name] = name_counter
                name = name_counter
                name_counter += 1
            new_floor.append(f"{typ} {name}")

        result.append(new_floor)

    return result


def test_normalize_floors():
    assert normalize_floors([["mc A"]]) == [["mc 1"]]
    assert normalize_floors([["mc asdf"]]) == [["mc 1"]]


def test_example_input():
    example = """
The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.
"""
    (part1, part2) = solve_for(example)

    assert part1 == 11
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 11))
    print(f"Part 1: {part1} | Part 2: {part2}")
