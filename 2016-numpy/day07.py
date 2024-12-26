import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def solve_for(input: str):
    lines = input.strip().splitlines()

    abba = r"(.)(?!\1)(.)\2\1"
    wrapped_abba = rf"\[[^]]*{abba}[^]]*\]"
    part1 = len(
        [l for l in lines if re.search(abba, l) and not re.search(wrapped_abba, l)]
    )

    part2 = len(
        [
            l
            for l in lines
            if re.search(
                r"(:?^|\])[a-z]*(.)(?!\2)(.)\2.*\[[a-z]*\3\2\3|\[[a-z]*(.)(?!\4)(.)\4.*\][a-z]*\5\4\5",
                l,
            )
        ]
    )

    return (part1, part2)


def test_example_input():
    example = """
abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn
aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb
aza[bbb]zaz
"""
    (part1, part2) = solve_for(example)

    assert part1 == 2
    assert part2 == 3


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 7))
    print(f"Part 1: {part1} | Part 2: {part2}")
