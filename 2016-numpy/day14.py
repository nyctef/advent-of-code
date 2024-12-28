from collections import defaultdict
import hashlib
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray
import itertools

threes_re = re.compile(r"(.)\1\1")
fives_re = re.compile(r"(.)\1\1\1\1")


def solve_for(input: str):
    salt = input.strip()

    threes = defaultdict(list)
    fives = defaultdict(list)

    part1 = ""

    for i in itertools.count():
        hash = hashlib.md5(f"{salt}{i}".encode("ascii")).hexdigest()

        if m := threes_re.search(hash):
            # print(f"found three {m.group(1)} at index {i}")
            threes[m.group(1)].append(i)
        if m := fives_re.search(hash):
            print(f"found five {m.group(1)} at index {i}")
            fives[m.group(1)].append(i)

            validated_threes = []
            for num, three_indexes in threes.items():
                for ti in three_indexes:
                    if any(fi for fi in fives[num] if ti < fi and ti > fi - 1001):
                        validated_threes.append(ti)
            print(f"num validated threes: {len(validated_threes)}")
            if len(validated_threes) >= 64:
                validated_threes.sort()
                part1 = validated_threes[63]
                break

    part2 = ""

    return (part1, part2)


def test_example_input():
    example = """abc"""
    (part1, part2) = solve_for(example)

    assert part1 == 22728
    assert part2 == ""


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 14))
    print(f"Part 1: {part1} | Part 2: {part2}")
