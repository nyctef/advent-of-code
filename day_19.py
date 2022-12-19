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

    def all_greater(self, other: "SearchStep"):
        return (
            self.ore_count >= other.ore_count
            and self.clay_count >= other.clay_count
            and self.obsidian_count >= other.obsidian_count
            and self.geode_count >= other.geode_count
            and self.ore_robot_count >= other.ore_robot_count
            and self.clay_robot_count >= other.clay_robot_count
            and self.obsidian_robot_count >= other.obsidian_robot_count
            and self.geode_robot_count >= other.geode_robot_count
        )

    def simulate_minute(self):
        return self._replace(
            after_minute=self.after_minute + 1,
            ore_count=self.ore_count + self.ore_robot_count,
            clay_count=self.clay_count + self.clay_robot_count,
            obsidian_count=self.obsidian_count + self.obsidian_robot_count,
            geode_count=self.geode_count + self.geode_robot_count,
        )

    def can_buy_ore_robot(self, bp: Blueprint):
        return self.ore_count >= bp.ore_robot_ore_cost

    def buy_ore_robot(self, bp: Blueprint):
        return self._replace(
            ore_count=self.ore_count - bp.ore_robot_ore_cost,
            ore_robot_count=self.ore_robot_count + 1,
        )

    def can_buy_clay_robot(self, bp: Blueprint):
        return self.ore_count >= bp.clay_robot_ore_cost

    def buy_clay_robot(self, bp: Blueprint):
        return self._replace(
            ore_count=self.ore_count - bp.clay_robot_ore_cost,
            clay_robot_count=self.clay_robot_count + 1,
        )

    def can_buy_obsidian_robot(self, bp: Blueprint):
        return (
            self.ore_count >= bp.obsidian_robot_ore_cost
            and self.clay_count >= bp.obsidian_robot_clay_cost
        )

    def buy_obsidian_robot(self, bp: Blueprint):
        return self._replace(
            ore_count=self.ore_count - bp.obsidian_robot_ore_cost,
            clay_count=self.clay_count - bp.obsidian_robot_clay_cost,
            obsidian_robot_count=self.obsidian_robot_count + 1,
        )

    def can_buy_geode_robot(self, bp: Blueprint):
        return (
            self.ore_count >= bp.geode_robot_ore_cost
            and self.obsidian_count >= bp.geode_robot_obsidian_cost
        )

    def buy_geode_robot(self, bp: Blueprint):
        return self._replace(
            ore_count=self.ore_count - bp.geode_robot_ore_cost,
            obsidian_count=self.obsidian_count - bp.geode_robot_obsidian_cost,
            geode_robot_count=self.geode_robot_count + 1,
        )


def simulate(blueprint: Blueprint, total_time: int):
    min1 = SearchStep(1, 1, 0, 0, 0, 1, 0, 0, 0)
    best_after_each_min = [min1] * (total_time + 1)

    q: List[SearchStep] = []
    q.append(min1)
    while q:
        n = q.pop()
        pprint(n)
        if n.all_greater(best_after_each_min[n.after_minute]):
            print(f"found a new best for min {n}")
            best_after_each_min[n.after_minute] = n

        if best_after_each_min[n.after_minute].all_greater(n):
            # we're in a strictly worse state, so this branch isn't worth considering
            print("halting early")
            continue

        if n.after_minute == total_time:
            print("done")
            continue

        # one option is always to do nothing
        n2 = n.simulate_minute()
        q.append(n2)

        # note checking whether we can buy at the start of the minute, but actually buying after the minute
        if n.can_buy_clay_robot(blueprint):
            q.append(n2.buy_clay_robot(blueprint))

    return best_after_each_min


def main():
    input = read_input("example")
    parsed = parse_input(input)
    bp0 = parsed[0]
    scores = simulate(bp0, 5)
    pprint(scores)


if __name__ == "__main__":
    main()
