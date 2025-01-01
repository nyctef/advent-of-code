from collections import defaultdict
from dataclasses import dataclass
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    part1 = run(lines)
    part2 = ""

    return (part1, part2)


def isnum(val: str):
    return val.lstrip("-").isdigit()


def run(lines):
    registers = defaultdict(lambda: 0)
    registers["a"] = 7
    pc = 0
    ctr = 1
    while pc < len(lines) and pc >= 0:
        (instr, x, *y) = lines[pc].split(" ")
        y = y[0] if len(y) else None
        ctr += 1
        if ctr % 1_000_001 == 0:
            print(f"{lines=}")
            print(f"{pc=} {instr=} {x=} {y=} {registers.items()=}")
            print()
        match instr:
            case "cpy":
                if not str.isalpha(y):
                    pc += 1
                    continue
                registers[y] = int(x) if isnum(x) else registers[x]
                pc += 1
            case "inc":
                if not str.isalpha(x):
                    pc += 1
                    continue
                registers[x] += 1
                pc += 1
            case "dec":
                if not str.isalpha(x):
                    pc += 1
                    continue
                registers[x] -= 1
                pc += 1
            case "jnz":
                val = int(x) if isnum(x) else registers[x]
                jmp = int(y) if isnum(y) else registers[y]
                if val != 0:
                    pc += jmp
                else:
                    pc += 1
            case "tgl":
                val = int(x) if isnum(x) else registers[x]
                target = pc + val
                if target < 0 or target >= len(lines):
                    pc += 1
                    continue
                (new_instr, x, *y) = lines[target].split(" ")
                y = y[0] if len(y) else None
                match new_instr:
                    case "inc":
                        lines[target] = f"dec {x}"
                    case "dec" | "tgl":
                        lines[target] = f"inc {x}"
                    case "jnz":
                        lines[target] = f"cpy {x} {y}"
                    case "cpy":
                        lines[target] = f"jnz {x} {y}"
                pc += 1

    return registers["a"]


def test_example_input():
    example = """
cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a
"""
    (part1, part2) = solve_for(example)

    assert part1 == 3
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 23))
    print(f"Part 1: {part1} | Part 2: {part2}")
