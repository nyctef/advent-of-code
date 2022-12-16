from collections import defaultdict, namedtuple
from pprint import pprint
from pathlib import Path
import re
from typing import Dict, List, Tuple

"""

started around 7am

open DD at 2  -> (30-2=28) * 20 = 560
open BB at 5  -> 25 * 13 = 325
open JJ at 9  -> 21 * 21 = 441
open HH at 17 -> 13 * 22 = 286
open EE at 21 -> 9 * 3 = 27
open CC at 24 -> 6 * 2 = 12
total: 1651

can we do dynamic programming to it?

simplest question: what to do at minute 30
it doesn't matter, since moving won't do anything, and opening a valve wont do anything until minute 31
next simplest question: what to do at minute 29
can either open a valve (gives extra 1*flow at minute 30) or move to another node (for which we've calculated the score previously)
tricky bit: how do we handle only being able to open a valve once?
probably can't just keep track of overall pressure released

tracked state probably needs to be (valve, minute opened at) pairs
so if we open the same valve earlier, we can increase the score appropriately

also how do we track which nodes are reachable at a given minute?
do we need to?
for each minute we calculate scores for every node, but the only viable path is the one that starts on node=AA at minute 1

does it matter that we think the score is 0 at min30, even though as we go back in time that'll increase?
or is it fine to always pick the biggest min30 score at min29, even though it'll be lower than the min29 score

how much space does this take?
54 nodes * 30 minutes * 10ish states (any valve with nonzero flow: on or off?)
should be fine1
"""


def read_input():
    input_file = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
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

    # input_file = Path("input/16-1.txt").read_text()
    return input_file


def splitc(s):
    return [x.strip() for x in s.split(",")]


Node = namedtuple("Node", "name, rate, tunnels")


def parse_input(input_file):
    inputs = input_file.splitlines()
    inputs = [
        re.fullmatch(
            r"^Valve (?P<v>..) has flow rate=(?P<f>\d+); tunnels? leads? to valves? (?P<t>[\w\s,]+)$",
            line,
        )
        for line in inputs
    ]
    inputs = [
        Node(m.group("v"), int(m.group("f")), splitc(m.group("t"))) for m in inputs
    ]
    return inputs


def print_inputs_as_dot(inputs, extra):
    print("digraph {")

    for i in inputs:
        print(f'{i[0]} [label="{i[0]}={i[1]} {extra.get(i[0], "")}"]')

    for i in inputs:
        for dest in i[2]:
            print(f"{i[0]} -> {dest}")

    print("}")


"""


def bfs(inputs: List[Node], start_node: str):
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


def get_expected_values(inputs: List[Node], steps_to_get_to, time_remaining):
    expected_values = {}
    for input in inputs:
        distance = steps_to_get_to[input.name]
        time_remaining_after_travel = time_remaining - distance
        time_remaining_after_opening = time_remaining_after_travel - 1
        expected_values[input.name] = time_remaining_after_opening * input.rate
    return expected_values

"""

# state: list of valves, flow rates and at what minute they were turned on
State = List[Tuple[str, int, int]]


def find_valve_named(inputs: List[Node], name: str):
    return next(x for x in inputs if x.name == name)


def score_state(s: State):
    return sum(flow * (30 - turned_on_at) for (valve, flow, turned_on_at) in s)


def open_valve(inputs: List[Node], old_state: State, valve: str, minute: int) -> State:
    new_state = [x for x in old_state if x[0] != valve]
    v = find_valve_named(inputs, valve)
    return new_state + [(valve, v.rate, minute)]


# minute -> valve name -> (action, resulting_state)
# action is either a valve name to move to or "OPEN"
BestChoicesAtMin = Dict[str, Tuple[str, State]]
BestChoices = Dict[int, BestChoicesAtMin]


def score_states(bcm: BestChoicesAtMin):
    result = {}
    for v, (action, state) in bcm.items():
        result[v] = score_state(state)
    return result


def calculate_best_choices_at(inputs: List[Node], bc: BestChoices, min: int):
    bc[min] = {}
    if min == 30:
        for input in inputs:
            # at minute 30, it doesn't matter what we do, so just pick a random action with zero value
            bc[min][input.name] = ("OPEN", [(input.name, input.rate, 30)])
    else:
        for input in inputs:
            best_action = "OPEN"
            best_action_score = -1
            best_state = []

            for tunnel in input.tunnels:
                (_, resulting_state) = bc[min + 1][tunnel]
                resulting_score = score_state(resulting_state)
                if resulting_score > best_action_score:
                    best_action = tunnel
                    best_action_score = resulting_score
                    best_state = resulting_state

            next_min_state_at_this_node = bc[min + 1][input.name][1]
            state_from_opening = open_valve(
                inputs, next_min_state_at_this_node, input.name, min
            )
            score_from_opening = score_state(state_from_opening)
            if score_from_opening > best_action_score:
                best_action = "OPEN"
                best_action_score = score_from_opening
                best_state = state_from_opening

            bc[min][input.name] = (best_action, best_state)


if __name__ == "__main__":
    input_file = read_input()
    inputs = parse_input(input_file)

    best_choices_at: BestChoices = {}

    calculate_best_choices_at(inputs, best_choices_at, 30)
    calculate_best_choices_at(inputs, best_choices_at, 29)
    calculate_best_choices_at(inputs, best_choices_at, 28)
    print_inputs_as_dot(inputs, score_states(best_choices_at[28]))
