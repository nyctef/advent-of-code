from collections import defaultdict, namedtuple
from pprint import pprint
from pathlib import Path
import re
from typing import List, Tuple


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


def bfs(inputs: List[Node], start_node):
    steps_to_get_to = defaultdict(lambda: 99999)
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


if __name__ == "__main__":
    input_file = read_input()
    inputs = parse_input(input_file)

    steps_to_get_to = bfs(inputs, "AA")
    print_inputs_as_dot(inputs, steps_to_get_to)
