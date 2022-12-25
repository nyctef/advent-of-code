from itertools import zip_longest
from math import log
from pathlib import Path
from typing import Iterator


def read_input(name: str):
    match name:
        case "example":
            return """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"""
        case "puzzle":
            return Path("input/25-1.txt").read_text()
        case other:
            raise Exception(other)


snafu_lookup = {"=": -2, "-": -1, "0": 0, "1": 1, "2": 2}
reverse_lookup = {v: k for k, v in snafu_lookup.items()}


def snafu_to_dec(x: str):
    total = 0
    e = 1
    for i in reversed(range(len(x))):
        total += e * (snafu_lookup[x[i]])
        e *= 5
    return total


def snafu_to_dec_digits(x: str):
    return [snafu_lookup[i] for i in x]


def dec_digits_to_snafu(result: Iterator[int]):
    return "".join(reverse_lookup[x] for x in result)


def add_snafu(a: str, b: str):
    # match digits together in order from least significant to most
    matched_digits = list(
        zip_longest(
            reversed(snafu_to_dec_digits(a)),
            reversed(snafu_to_dec_digits(b)),
            fillvalue=0,
        )
    )
    # starting from the least significant digit, add values together
    # and carry upwards if necessary
    result: list[int] = []
    carry = 0
    for a1, b1 in matched_digits:
        n = a1 + b1 + carry
        carry = 0
        if n > 2:
            n -= 5
            carry += 1
        elif n < -2:
            n += 5
            carry -= 1

        result.append(n)

    # if there's value left over, it becomes the new most significant digit
    if carry != 0:
        result.append(carry)

    # then reverse the digits back again so the most significant is first,
    # and convert decimal digits back to a snafu string
    return dec_digits_to_snafu(reversed(result))


def main():
    i = read_input("puzzle").splitlines()
    total = "0"
    for n in i:
        total = add_snafu(total, n)
    print(total)
    assert snafu_to_dec(total) == sum(snafu_to_dec(x) for x in i)


main()
