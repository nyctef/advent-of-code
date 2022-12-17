from collections import defaultdict, namedtuple
from itertools import chain, combinations
from pprint import pprint
from pathlib import Path
import re
import statistics
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
        TargetLocation(c.name, i, c.rate, 1 << i) for (i, c) in enumerate(target_caves)
    ]

    distances = [[999] * len(locations) for _ in range(len(locations))]

    for source in locations:
        distances_from_source = search_distances(caves, source.name)
        for target in locations:
            distances[source.index][target.index] = distances_from_source[target.name]

    num_valve_states = 2 ** len(target_caves)
    return World(time_limit, locations, distances, num_valve_states)


############ Part 2: dynamic programming on the weighted graph


def is_valve_open(valve_state: int, loc_index: int):
    return valve_state & (1 << loc_index)


def calculate_best_choices_at(world: World, min: int, bc: BestChoices):
    aa = next(l for l in world.locations if l.name == "AA")
    if min == world.last_minute:
        # at minute 30, it doesn't matter what we do, so just pick a random action with zero value
        do_nothing = Choice("wait", "wait", [])
        for me_location in range(len(world.locations)):
            for ele_location in range(len(world.locations)):
                for valve_state in range(world.num_valve_states):
                    bc[min][
                        WorldState(me_location, ele_location, valve_state)
                    ] = do_nothing
    else:
        count = 0
        possible_me_locations = list(range(len(world.locations)))
        possible_ele_locations = list(range(len(world.locations)))
        if min != 1:
            # if it's not minute 1, we have no reason to be at AA any more
            possible_me_locations.remove(aa.index)
            possible_ele_locations.remove(aa.index)
        for me_location in range(len(world.locations)):
            for ele_location in range(len(world.locations)):
                for valve_state in range(world.num_valve_states):
                    # -1 because no reason to head to AA
                    possible_me_targets = (
                        t
                        for t in range(len(world.locations))
                        if t != aa.index and not is_valve_open(valve_state, t)
                    )
                    possible_ele_targets = (
                        t
                        for t in range(len(world.locations))
                        if t != aa.index and not is_valve_open(valve_state, t)
                    )
                    for me_target in possible_me_targets:
                        for ele_target in possible_ele_targets:
                            count += 1
                            if count % 1_000_000 == 0:
                                print(count)
                            if ele_target == me_target:
                                continue
                            do_nothing = Choice("wait", "wait", [])
                            bc[min][
                                WorldState(me_location, ele_location, valve_state)
                            ] = do_nothing


def run_back_in_time(world: World):
    best_choices_per_min: BestChoices = [{} for _ in range(world.last_minute + 1)]
    for min in reversed(range(1, world.last_minute + 1)):
        print("------")
        print(min)
        calculate_best_choices_at(world, min, best_choices_per_min)
    return best_choices_per_min


class QueueState(NamedTuple):
    me_time_remaining: int
    ele_time_remaining: int
    current_me_location: int
    current_ele_location: int
    visited_locations: int


def get_possible_next_states(world: World, prev_state: QueueState):
    result: List[QueueState] = []
    for possible_me_target in range(len(world.locations) - 1):
        if is_valve_open(prev_state.visited_locations, possible_me_target):
            continue

        me_time_taken = (
            world.distances[prev_state.current_me_location][possible_me_target] + 1
        )
        if me_time_taken > prev_state.me_time_remaining:
            continue

        for possible_ele_target in range(len(world.locations) - 1):
            if possible_me_target == possible_ele_target:
                continue

            if is_valve_open(prev_state.visited_locations, possible_ele_target):
                continue

            ele_time_taken = (
                world.distances[prev_state.current_ele_location][possible_ele_target]
                + 1
            )
            if ele_time_taken > prev_state.ele_time_remaining:
                continue

            new_visited_locations = prev_state.visited_locations
            new_visited_locations |= world.locations[possible_me_target].flag
            new_visited_locations |= world.locations[possible_ele_target].flag
            result.append(
                QueueState(
                    prev_state.me_time_remaining - me_time_taken,
                    prev_state.ele_time_remaining - ele_time_taken,
                    possible_me_target,
                    possible_ele_target,
                    new_visited_locations,
                )
            )
    return result


def search_for_best_ordering(world: World):
    """

    want to be able to generate all (me, ele) sequences which don't last longer than last_min
    first problem: how many of these sequences are there?

    - start at AA (state=(visited:AA, loc_remaining: BB,CC,DD..., me_time_remaining:30, ele_time_remaining: 30))
      - each sub-sequence will pick a me action and/or an ele action, until both are out of time

    """
    aa = next(l for l in world.locations if l.name == "AA")
    assert aa.index == len(world.locations) - 1

    initial_state = QueueState(
        world.last_minute, world.last_minute, aa.index, aa.index, 0
    )
    next_states = get_possible_next_states(world, initial_state)
    pprint(next_states)


############ Part 3: main


def main():
    input_file = read_input("example")
    parsed_caves = parse_input(input_file)
    world = build_world(parsed_caves, 26)
    pprint(world.locations)
    pprint(world.distances)
    aa_index = next(x.index for x in world.locations if x.name == "AA")
    # bc = run_back_in_time(world)
    # print(bc[1][WorldState(aa_index, aa_index, 0)].resulting_state)
    print(statistics.fmean([statistics.fmean(x) for x in world.distances]))
    search_for_best_ordering(world)


if __name__ == "__main__":
    main()
