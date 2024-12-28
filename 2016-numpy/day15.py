import math
import re
from util import get_input
import numpy as np
from numpy.typing import NDArray


def egcd(a, b):
    """
    https://stackoverflow.com/a/42978824
    from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Python

    takes two integers a and b as input
    outputs (g, x, y) such that ax + by = g where g is the gcd of a and b


    """
    if a == 0:
        return (b, 0, 1)
    else:
        g, x, y = egcd(b % a, a)
        return (g, y - (b // a) * x, x)


def mod_mul_inverse(x, N):
    """
    egcd also lets us calculate the modular multiplicative inverse: that is to say, if we have a number
    x mod N, then the modular multiplicative inverse is defined as a number y where x * y mod N == 1.
    This number only exists if the gcd of x and N is 1.

    that is to say, xy + ?N = 1. So we can use egcd(x, N) to calculate (g, y, ?). If we assert that the
    resulting g is 1 then we can say the multiplicative inverse of x is y
    """
    (g, y, _) = egcd(x, N)
    assert g == 1
    return y


def solve_for(input: str, part2: bool):
    lines = input.strip().splitlines()
    if part2:
        lines.append("Disc #7 has 11 positions; at time=0, it is at position 0.")
    digits_re = re.compile(r"\d+")
    discs = []
    for line in lines:
        (disc, positions, _zero, initial) = tuple(
            int(x) for x in digits_re.findall(line)
        )
        # the disc starts in position `initial` at t=0
        # at button-pressing time, we want to each disc to be at zero
        # minus its number (disc 1 should be at position -1, disc 2 at
        # -2, etc)
        target = positions - disc
        distance = (target - initial) % positions
        discs.append((positions, distance))

    print(discs)
    # https://brilliant.org/wiki/chinese-remainder-theorem/
    as_ = [distance for (_, distance) in discs]
    ns = [positions for (positions, _) in discs]
    large_mod = math.prod(n for n in ns)
    ys = [int(large_mod / n) for n in ns]
    zs = [mod_mul_inverse(y, n) for (y, n) in zip(ys, ns)]

    result = sum([a * y * z for (a, y, z) in zip(as_, ys, zs)])
    while result < 0:
        result += large_mod

    return result


def test_example_input():
    example = """
Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
"""
    part1 = solve_for(example, False)

    assert part1 == 5


if __name__ == "__main__":
    part1 = solve_for(get_input(2016, 15), False)
    part2 = solve_for(get_input(2016, 15), True)
    print(f"Part 1: {part1} | Part 2: {part2}")
