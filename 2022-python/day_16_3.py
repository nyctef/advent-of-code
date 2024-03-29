from collections import defaultdict, namedtuple
from itertools import chain, combinations
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
54 nodes * 100ish previous states * 30 minutes * 10ish states (any valve with nonzero flow: on or off?)
~should be fine~

think the next problem is: maybe we need to calculate the next best move at min n, assuming any possible combination of states at min n - 1?


AA(0) -> BB(1)
so eg at min 29

"""

last_min = 26


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
    input_file = Path("input/16-1.txt").read_text()
    return input_file

    return input_file

    input_file = """Valve AA has flow rate=0; tunnels lead to valves BB
Valve BB has flow rate=1; tunnels lead to valves AA"""


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


HasAlreadyBeenOpened = Dict[str, bool]


def generate_all_previous_states(valve_names: List[str]):
    result: List[HasAlreadyBeenOpened] = []
    for nameset in powerset(valve_names):
        n = {}
        for name in valve_names:
            n[name] = name in nameset
    return result


def powerset(valve_names: List[str]):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    return chain.from_iterable(
        combinations(valve_names, r) for r in range(len(valve_names) + 1)
    )


# state: list of valves, flow rates and at what minute they were turned on
State = List[Tuple[str, int, int]]

# minute -> valve name -> [(action, resulting_state)]
# action is either a valve name to move to or "OPEN"
# the list is indexed by every possible previous state of each valve (on, off)
BestChoicesAtMin = Dict[str, List[Tuple[str, State]]]
BestChoices = List[BestChoicesAtMin]


def summarize_state(s: State) -> HasAlreadyBeenOpened:
    result = defaultdict(lambda: False)
    for (name, flow, min) in s:
        result[name] = True
    return result


def find_valve_named(inputs: List[Node], name: str):
    return next(x for x in inputs if x.name == name)


def score_state(s: State):
    return sum(flow * (last_min - turned_on_at) for (valve, flow, turned_on_at) in s)


def open_valve(inputs: List[Node], old_state: State, valve: str, minute: int) -> State:
    new_state = [x for x in old_state if x[0] != valve]
    v = find_valve_named(inputs, valve)
    return new_state + [(valve, v.rate, minute)]


def score_states(bcm: BestChoicesAtMin):
    result = {}
    # when doing final scoring, pick the 0th possible state since we're scoring min 0, when we know that no valves will be open
    for v, [(action, state), *rest] in bcm.items():
        result[v] = score_state(state)
    return result


def is_valve_set_already(
    valve_state: int, valve_indexes: Dict[str, int], target_valve: str
):
    valve_index = valve_indexes[target_valve]
    return valve_state & (1 << valve_index) != 0


def update_valve_state_if_opening_valve(
    valve_state: int, valve_indexes: Dict[str, int], target_valve: str
):
    valve_index = valve_indexes[target_valve]
    return valve_state | (1 << valve_index)


def nothing(*args):
    pass


log = nothing


def calculate_best_choices_at(
    inputs: List[Node], bc: BestChoices, min: int, working_valves: List[str]
):
    num_valve_states = 2 ** len(working_valves)
    bc[min] = {}
    if min == last_min:
        for input in inputs:
            bc[min][input.name] = []
            for valve_state in range(num_valve_states):
                # at minute last_min, it doesn't matter what we do, so just pick a random action with zero value
                bc[min][input.name].append(
                    ("OPEN", [(input.name, input.rate, last_min)])
                )
    else:
        for input in inputs:
            log()
            log(f"now looking at valve {input.name}")
            bc[min][input.name] = [None] * num_valve_states

            for valve_state in range(num_valve_states):
                best_action = "nothing?"
                best_action_score = -1
                best_state = []

                log(f"considering valve state {valve_state}")
                # valve_b_set = is_valve_set_already(valve_state, valve_indexes, "BB")
                # log(f"valve BB is {valve_b_set}")
                for tunnel in input.tunnels:
                    (_, resulting_state) = bc[min + 1][tunnel][valve_state]
                    resulting_score = score_state(resulting_state)
                    log(f"considering moving to {tunnel=} {resulting_score=}")
                    if resulting_score > best_action_score:
                        best_action = tunnel
                        best_action_score = resulting_score
                        best_state = resulting_state

                if input.name in working_valves and not (
                    is_valve_set_already(valve_state, valve_indexes, input.name)
                ):
                    log(f"considering opening valve {input.name}")
                    updated_valve_state_from_opening = (
                        update_valve_state_if_opening_valve(
                            valve_state, valve_indexes, input.name
                        )
                    )
                    (_, next_min_state_at_this_node) = bc[min + 1][input.name][
                        updated_valve_state_from_opening
                    ]
                    state_from_opening = open_valve(
                        inputs, next_min_state_at_this_node, input.name, min
                    )
                    score_from_opening = score_state(state_from_opening)
                    if score_from_opening > best_action_score:
                        best_action = "OPEN"
                        best_action_score = score_from_opening
                        best_state = state_from_opening

                log(
                    f"best action for valve {input.name} in state {valve_state} is {best_action} with score {best_action_score}"
                )

                bc[min][input.name][valve_state] = (best_action, best_state)
    log(f"min {min}: {bc[min]}")


if __name__ == "__main__":
    input_file = read_input()
    inputs = parse_input(input_file)

    working_valves = [i.name for i in inputs if i.rate > 0]
    valve_indexes = {n: i for (i, n) in enumerate(working_valves)}
    # print(len(inputs))
    # print(working_valves)
    # print(valve_indexes)
    # print(last_min * len(inputs) * 2 ** len(working_valves))

    best_choices_at: BestChoices = [{} for _ in range(last_min + 1)]

    # for min in reversed(range(1, last_min + 1)):
    for min in reversed(range(1, last_min + 1)):
        print("---------")
        print(min)
        calculate_best_choices_at(inputs, best_choices_at, min, working_valves)
        # pprint(best_choices_at[min], width=140)
    scores = score_states(best_choices_at[1])
    # print_inputs_as_dot(inputs, scores)
    print(f"my_score: AA -> {scores['AA']}")
    my_score = scores["AA"]
    my_valves = [v[0] for v in best_choices_at[1]["AA"][0][1]]

    # cheat: apparently in the puzzle input, the best solution involves me and the elephant going in divergent paths
    # so I end up going in the best original path, and the elephant ends up going in a second-best path which doesn't overlap
    working_valves = [v for v in working_valves if v not in my_valves]
    valve_indexes = {n: i for (i, n) in enumerate(working_valves)}
    best_choices_at: BestChoices = [{} for _ in range(last_min + 1)]
    for min in reversed(range(1, last_min + 1)):
        print("---------")
        print(min)
        calculate_best_choices_at(inputs, best_choices_at, min, working_valves)

    scores = score_states(best_choices_at[1])
    # print_inputs_as_dot(inputs, scores)
    print(f"ele_score: AA -> {scores['AA']}")
    ele_score = scores["AA"]
    ele_valves = [v[0] for v in best_choices_at[1]["AA"][0][1]]

    print(f"{my_score=} {my_valves=} {ele_score=} {ele_valves=}")
    print(my_score + ele_score)
