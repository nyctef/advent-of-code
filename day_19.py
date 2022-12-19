from pathlib import Path
from pprint import pprint
import re


def read_input(name: str):
    match name:
        case "example":
            return """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
        case "puzzle":
            return Path("input/19-1.txt").read_text()
        case other:
            raise Exception(other)


def parse_input(input: str):
    result = []
    for line in input.splitlines():
        bp, costs = line.split(":")
        bpid = int(bp.split(" ")[1])
        costs_ = [x for x in costs.split(".") if x.strip() != ""]
        costs_ = [x.strip() for x in costs_]
        costs = {}
        for cost_line in costs_:
            # print(cost_line)
            m = re.match(
                r"Each ([a-z]+ robot) costs (\d+) ore( and (\d+ [a-z]+))?", cost_line
            )
            assert m is not None
            robot_type = m.group(1)
            ore_cost = int(m.group(2))
            costs[robot_type] = []
            costs[robot_type].append(("ore", ore_cost))
            other_cost = m.group(4)
            if other_cost is not None:
                other_cost_type = other_cost.split(" ")[1]
                other_cost_num = int(other_cost.split(" ")[0])
                costs[robot_type].append((other_cost_type, other_cost_num))
        result.append((bpid, costs))
    return result


def main():
    input = read_input("example")
    parsed = parse_input(input)
    pprint(parsed)


if __name__ == "__main__":
    main()
