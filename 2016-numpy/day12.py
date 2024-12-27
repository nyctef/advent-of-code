from collections import defaultdict
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    registers = defaultdict(lambda: 0)
    pc = 0
    while pc < len(lines) and pc >= 0:
        (instr, x, *y) = lines[pc].split(" ")
        y = y[0] if len(y) else None
        # print(instr, x, y, registers)
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
                if registers[x] != 0:
                    pc += int(y)
                else:
                    pc += 1

    part1 = registers["a"]
    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """
cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
"""
    (part1, part2) = solve_for(example)

    assert part1 == 42
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 12))
    print(f"Part 1: {part1} | Part 2: {part2}")
