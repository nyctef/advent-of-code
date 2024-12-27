from collections import defaultdict
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str, part2: bool):
    lines = input.strip().splitlines()

    registers = defaultdict(lambda: 0)
    if part2:
        registers["c"] = 1
    pc = 0
    while pc < len(lines) and pc >= 0:
        (instr, x, *y) = lines[pc].split(" ")
        y = y[0] if len(y) else None
        # print(instr, x, y, registers.items())
        match instr:
            case "cpy":
                registers[y] = int(x) if x.isdigit() else registers[x]
                pc += 1
            case "inc":
                registers[x] += 1
                pc += 1
            case "dec":
                registers[x] -= 1
                pc += 1
            case "jnz":
                val = int(x) if x.isdigit() else registers[x]
                if val != 0:
                    pc += int(y)
                else:
                    pc += 1

    return registers["a"]


def test_example_input():
    example = """
cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
"""
    part1 = solve_for(example, False)

    assert part1 == 42


if __name__ == "__main__":
    part1 = solve_for(get_input(2016, 12), False)
    part2 = solve_for(get_input(2016, 12), True)
    print(f"Part 1: {part1} | Part 2: {part2}")
