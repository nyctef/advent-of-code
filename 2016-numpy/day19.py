from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    elf_count = int(input)

    elfs = [1 for _ in range(elf_count)]

    def next_elf(i):
        return (i + 1) % elf_count

    ptr = 0
    while True:
        while elfs[ptr] == 0:
            ptr = next_elf(ptr)
        next_elf_ptr = next_elf(ptr)
        while elfs[next_elf_ptr] == 0:
            next_elf_ptr = next_elf(next_elf_ptr)

        print(
            f"elf {ptr+1} takes {elfs[next_elf_ptr]} presents from elf {next_elf_ptr+1}"
        )
        elfs[ptr] += elfs[next_elf_ptr]
        elfs[next_elf_ptr] = 0

        if elfs[ptr] == elf_count:
            part1 = ptr + 1
            break

        ptr = next_elf(ptr)

    part2 = ""

    return (part1, part2)


def test_example_input():
    assert solve_for("5")[0] == 3
    assert solve_for("7")[0] == 5

    assert part1 == 3
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 19))
    print(f"Part 1: {part1} | Part 2: {part2}")
