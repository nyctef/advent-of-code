import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    input = input.strip().splitlines()

    lines = [
        re.search(r"^([a-z-]+)(\d+)\[([a-z]+)\]$", line).groups() for line in input
    ]
    part1 = 0
    for room, sector, checksum in lines:
        room = np.array(list(room), dtype=str)
        room = room[room != "-"]
        (values, counts) = np.unique_counts(room)
        # for lexsort, the *last* axis is the primary axis to sort by
        indexes = np.lexsort((values, -counts))[:5]
        if "".join(values[indexes]) == checksum:
            part1 += int(sector)

    for room, sector, checksum in lines:
        chars = list(room)
        rotation = int(sector) % 26
        for i in range(len(chars)):
            if chars[i].isalpha():
                char = ord(chars[i]) - ord("a")
                char = (char + rotation) % 26
                char = char + ord("a")
                chars[i] = chr(char)
        decrypted_name = "".join(chars)
        if "north" in decrypted_name:
            part2 = sector

    return (part1, part2)


def test_example_input():
    example = """
aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]
"""
    (part1, part2) = solve_for(example)

    assert part1 == 1514
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 4))
    print(f"Part 1: {part1} | Part 2: {part2}")
