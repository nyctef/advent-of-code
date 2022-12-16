from pprint import pprint
from pathlib import Path
import re


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


def parse_input(input_file):
    inputs = input_file.splitlines()
    inputs = [
        re.fullmatch(
            r"^Valve (?P<v>..) has flow rate=(?P<f>\d+); tunnels? leads? to valves? (?P<t>[\w\s,]+)$",
            line,
        )
        for line in inputs
    ]
    inputs = [(m.group("v"), int(m.group("f")), splitc(m.group("t"))) for m in inputs]
    return inputs


def print_inputs_as_dot(inputs):
    print("digraph {")

    for i in inputs:
        print(f'{i[0]} [label="{i[0]}={i[1]}"]')

    for i in inputs:
        for dest in i[2]:
            print(f"{i[0]} -> {dest}")

    print("}")


if __name__ == "__main__":
    input_file = read_input()
    inputs = parse_input(input_file)

    print_inputs_as_dot(inputs)
