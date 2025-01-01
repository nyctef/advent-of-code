from collections import defaultdict
import itertools
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    target = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
    for a in itertools.count(2000):
        if a % 1000 == 0:
            print(a)
        out = []
        run(lines, a, out)
        # print(out)
        if out[:10] == target or out[1:11] == target:
            print(out)
            print(a)
            break

    return (part1, part2)


def run(lines, start_a, out: list):
    registers = defaultdict(lambda: 0)
    registers["a"] = start_a
    pc = 0
    while pc < len(lines) and pc >= 0:
        (instr, x, *ys) = lines[pc].split(" ")
        y = ys[0] if len(ys) else None
        z = ys[1] if len(ys) > 1 else None
        match instr:
            case "movmuladd":
                a = registers[x]
                b = registers[y]
                c = registers[z]
                registers[x] = 0
                registers[y] = 0
                registers[z] = c + a * b
                pc += 1
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
            case "out":
                val = int(x) if x.isdigit() else registers[x]
                out.append(val)
                if (len(out) > 1 and out[0] == out[1]) or len(out) > 20:
                    return


def test_example_input():
    example = """

"""
    (part1, part2) = solve_for(example)

    assert part1 == ""
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 25))
    print(f"Part 1: {part1} | Part 2: {part2}")
