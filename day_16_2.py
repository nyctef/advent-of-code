from collections import defaultdict, namedtuple
from itertools import chain, combinations
from pprint import pprint
from pathlib import Path
import re
from typing import Dict, List, Literal, NamedTuple, Tuple


class ParsedCave(NamedTuple):
    name: str
    rate: int
    tunnels: List[str]


class TargetLocation(NamedTuple):
    # human-readable (ish) name of this location
    name: str
    # index of this location in World.locations
    index: int
    rate: int
    # = 1<<index
    flag: int


class World(NamedTuple):
    # the time limit for this particular world
    last_minute: int
    # only AA and working valves are stored here
    locations: List[TargetLocation]
    # distances from location X to location Y indexed by TargetLocation.index
    distances: List[List[int]]
    # 2 ^ (len(locations) - 1) (since we don't need to count AA as a valve state)
    num_valve_states: int


class WorldState(NamedTuple):
    # these positions index into World.locations
    my_position: int
    ele_position: int
    # valve state is a bitfield of which World.locations valves are opened
    valve_state: int


class ScorableValveState(NamedTuple):
    # indexes into World.locations
    valve_index: int
    valve_rate: int
    # which minute the valve was turned on
    # the valve scores `valve_rate` points every minute after that one
    on_at_min: int


ScorableWorldState = List[ScorableValveState]


class Choice(NamedTuple):
    # action: either open a valve at index, or wait/travel to the next valve
    my_action: int | Literal["wait"]
    ele_action: int | Literal["wait"]
    resulting_state: ScorableWorldState


BestChoicesAtMin = Dict[WorldState, Choice]

BestChoices = List[BestChoicesAtMin]

############ Part 1: reading/parsing the full cave input and converting to a weighted graph


def read_input(name: str):
    match name:
        case "example":
            return """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""
        case "test-1":
            return """Valve AA has flow rate=0; tunnels lead to valves BB, CC
Valve BB has flow rate=1; tunnels lead to valves AA
Valve CC has flow rate=1; tunnels lead to valves AA
"""
        case "test-2":
            """
            AA - BA - BB(1)
             |
             > - CA - CB(1)
            """
            return """Valve AA has flow rate=0; tunnels lead to valves BA, CA
Valve BA has flow rate=0; tunnels lead to valves AA, BB
Valve CA has flow rate=0; tunnels lead to valves AA, CB
Valve BB has flow rate=1; tunnels lead to valves BA
Valve CB has flow rate=1; tunnels lead to valves CA
"""
        case "puzzle":
            return Path("input/16-1.txt").read_text()

        case other:
            raise Exception(f"Unknown input file {other}")


def parse_input(input_file: str):
    def splitc(s):
        return [x.strip() for x in s.split(",")]

    inputs = input_file.splitlines()
    inputs = [
        re.fullmatch(
            r"^Valve (?P<v>..) has flow rate=(?P<f>\d+); tunnels? leads? to valves? (?P<t>[\w\s,]+)$",
            line,
        )
        for line in inputs
    ]
    inputs = [
        ParsedCave(m.group("v"), int(m.group("f")), splitc(m.group("t"))) for m in inputs  # type: ignore (we know these re groups exist)
    ]
    return inputs


def search_distances(inputs: List[ParsedCave], start_node: str):
    steps_to_get_to: Dict[str, int] = defaultdict(lambda: 99999)
    q: List[Tuple[int, str]] = []
    q.append((0, start_node))
    while q:
        (dist, next_name) = q.pop()
        next_input = next(x for x in inputs if x.name == next_name)
        steps_to_get_to[next_name] = min(steps_to_get_to[next_name], dist)
        for t in next_input.tunnels:
            if steps_to_get_to[t] <= dist:
                continue
            q.append((dist + 1, t))

    return steps_to_get_to


def build_world(caves: List[ParsedCave], time_limit: int) -> World:
    working_valves = [c for c in caves if c.rate > 0]
    target_caves = working_valves + [next(c for c in caves if c.name == "AA")]

    locations = [
        TargetLocation(c.name, i, c.rate, 1 << 1) for (i, c) in enumerate(target_caves)
    ]

    distances = [[999] * len(locations) for _ in range(len(locations))]

    for source in locations:
        distances_from_source = search_distances(caves, source.name)
        for target in locations:
            distances[source.index][target.index] = distances_from_source[target.name]

    num_valve_states = 2 ** len(target_caves)
    return World(time_limit, locations, distances, num_valve_states)


############ Part 2: dynamic programming on the weighted graph


############ Part 3: main


def main():
    input_file = read_input("example")
    parsed_caves = parse_input(input_file)
    world = build_world(parsed_caves, 5)
    pprint(world.locations)
    pprint(world.distances)


if __name__ == "__main__":
    main()
