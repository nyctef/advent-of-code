from pathlib import Path
from pprint import pprint
import re
from typing import Dict, List, NamedTuple, Tuple


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
    result: List[Blueprint] = []
    for line in input.splitlines():
        bp, costs = line.split(":")
        bpid = int(bp.split(" ")[1])
        costs_ = [x for x in costs.split(".") if x.strip() != ""]
        costs_ = [x.strip() for x in costs_]

        bp = Blueprint(bpid, 0, 0, 0, 0, 0, 0)

        for cost_line in costs_:
            # print(cost_line)
            m = re.match(
                r"Each ([a-z]+) robot costs (\d+) ore( and (\d+ [a-z]+))?", cost_line
            )
            assert m is not None
            robot_type = m.group(1)
            ore_cost = int(m.group(2))
            match robot_type:
                case "ore":
                    bp = bp._replace(ore_robot_ore_cost=ore_cost)
                case "clay":
                    bp = bp._replace(clay_robot_ore_cost=ore_cost)
                case "obsidian":
                    bp = bp._replace(obsidian_robot_ore_cost=ore_cost)
                case "geode":
                    bp = bp._replace(geode_robot_ore_cost=ore_cost)
            # costs[robot_type].append(("ore", ore_cost))
            other_cost = m.group(4)
            if other_cost is not None:
                other_cost_type = other_cost.split(" ")[1]
                other_cost_num = int(other_cost.split(" ")[0])
                match other_cost_type:
                    case "clay":
                        bp = bp._replace(obsidian_robot_clay_cost=other_cost_num)
                    case "obsidian":
                        bp = bp._replace(geode_robot_obsidian_cost=other_cost_num)
                # costs[robot_type].append((other_cost_type, other_cost_num))
        result.append(bp)
    return result


class ItemCost(NamedTuple):
    name: str
    amount: int


class Blueprint(NamedTuple):
    id: int
    ore_robot_ore_cost: int
    clay_robot_ore_cost: int
    obsidian_robot_ore_cost: int
    obsidian_robot_clay_cost: int
    geode_robot_ore_cost: int
    geode_robot_obsidian_cost: int


class SearchStep(NamedTuple):
    after_minute: int
    ore_count: int
    clay_count: int
    obsidian_count: int
    geode_count: int
    ore_robot_count: int
    clay_robot_count: int
    obsidian_robot_count: int
    geode_robot_count: int


def main():
    input = read_input("example")
    parsed = parse_input(input)
    pprint(parsed)


if __name__ == "__main__":
    main()
