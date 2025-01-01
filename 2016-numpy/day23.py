from collections import defaultdict
from dataclasses import dataclass
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    for i in range(7, 13):
        print(f"{i} -> {run(list(lines), i)}")

    part1 = ""
    part2 = ""
    return (part1, part2)


def isnum(val: str):
    return val.lstrip("-").isdigit()


def run(lines, start_a):
    registers = defaultdict(lambda: 0)
    registers["a"] = start_a
    pc = 0
    ctr = 1
    while pc < len(lines) and pc >= 0:
        (instr, x, *ys) = lines[pc].split(" ")
        y = ys[0] if len(ys) else None
        z = ys[1] if len(ys) > 1 else None
        ctr += 1
        if ctr % 1_000_001 == 0 or pc == 10:
            print(f"{lines=}")
            print(f"{pc=} {instr=} {x=} {y=} {registers.items()=}")
            print()
        match instr:
            case "nop":
                pc += 1
            case "movadd":
                a = registers[x]
                b = registers[y]
                registers[x] = 0
                registers[y] = a + b
                pc += 1
            case "movmul":
                a = registers[x]
                b = registers[y]
                c = registers[z]
                registers[x] = 0
                # registers[y] = 0
                registers[z] = a * b
                pc += 1

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
                    # print(f"jumping {jmp} to {pc+jmp}")
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
                print(f"tgl {val=} {target=} {new_instr=} {x=} {y=}")
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
                    case _:
                        raise Exception(f"dont know how to toggle {new_instr}")
                pc += 1
            case _:
                raise Exception(f"don't know instruction {instr}")

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
    assert part2 == 3


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 23))
    print(f"Part 1: {part1} | Part 2: {part2}")
