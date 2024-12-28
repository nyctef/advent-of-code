from collections import defaultdict
import hashlib
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray
import itertools

threes_re = re.compile(r"(.)\1\1")
fives_re = re.compile(r"(.)\1\1\1\1")


def solve_for(input: str, part2: bool):
    salt = input.strip()

    threes = defaultdict(list)
    fives = defaultdict(list)

    for i in itertools.count():
        hash = hashlib.md5(f"{salt}{i}".encode("ascii")).hexdigest()

        if part2:
            for _ in range(2016):
                hash = hashlib.md5(hash.encode("ascii")).hexdigest()

        if m := threes_re.search(hash):
            # print(f"found three {m.group(1)} at index {i}")
            threes[m.group(1)].append(i)
        if m := fives_re.search(hash):
            print(f"found five {m.group(1)} at index {i}")
            fives[m.group(1)].append(i)

            validated_threes = []
            for num, three_indexes in threes.items():
                for ti in three_indexes:
                    if any(fi for fi in fives[num] if ti + 1 <= fi <= ti + 1001):
                        validated_threes.append(ti)
            print(f"num validated threes: {len(validated_threes)}")
            if len(validated_threes) >= 64:
                validated_threes.sort()
                candidate = validated_threes[63]
                if i > candidate + 1000:
                    # need to go for at least another 1000 hashes after our possible solution,
                    # since we might discover another five-match soon which would validate
                    # an earlier three-match
                    return candidate


def test_example_input():
    example = """abc"""
    part1 = solve_for(example, False)
    part2 = solve_for(example, True)

    assert part1 == 22728
    assert part2 == 22551


if __name__ == "__main__":
    part1 = "???"
    part1 = solve_for(get_input(2016, 14), False)
    part2 = solve_for(get_input(2016, 14), True)
    print(f"Part 1: {part1} | Part 2: {part2}")
