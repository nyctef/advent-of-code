from collections import defaultdict
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    outputs = defaultdict(list)
    bots = defaultdict(list)

    rules = {}

    for instr in lines:
        if m := re.match(r"value (\d+) goes to bot (\d+)", instr):
            (value, bot) = tuple(int(x) for x in m.groups())
            bots[bot].append(value)
        if m := re.match(
            r"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)",
            instr,
        ):
            (bot_, target1, num1_, target2, num2_) = m.groups()
            (bot, num1, num2) = (int(bot_), int(num1_), int(num2_))
            rules[bot] = (target1, num1, target2, num2)

    part1 = "???"
    work_remaining = True
    while work_remaining:
        work_remaining = False
        for bot, botvalues in list(bots.items()):
            if len(botvalues) >= 2:
                work_remaining = True
                assert len(botvalues) == 2
                values = list(botvalues)
                botvalues.clear()
                values.sort()

                if [17, 61] == values:
                    print(f"bot {bot} compared 17 with 61")
                    part1 = bot

                [lowvalue, highvalue] = values

                (target1, num1, target2, num2) = rules[bot]
                if target1 == "output":
                    outputs[num1].append(lowvalue)
                else:
                    bots[num1].append(lowvalue)
                if target2 == "output":
                    outputs[num2].append(highvalue)
                else:
                    bots[num2].append(highvalue)

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 10))
    print(f"Part 1: {part1} | Part 2: {part2}")
