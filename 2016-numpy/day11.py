import re
from util import get_input
import numpy as np
from numpy.typing import NDArray
import heapq
from copy import deepcopy


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


def next_states(state):
    (turns, _score, elevator, floors) = state


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
    state = (0, score(floors), 0, floors)

    part1 = ""
    part2 = ""

    return (part1, part2)


def test_floors_valid():
    assert is_valid([["mc A"]]) == True
    assert is_valid([["gen A"]]) == True
    assert is_valid([["mc A", "gen A"]]) == True
    assert is_valid([["mc A", "gen A", "gen B"]]) == True
    assert is_valid([["mc A", "gen B"]]) == False


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 11))
    print(f"Part 1: {part1} | Part 2: {part2}")
