from itertools import zip_longest
from pathlib import Path


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


s_lookup = {"=": -2, "-": -1, "0": 0, "1": 1, "2": 2}
lookup = {"=": 0, "-": 1, "0": 2, "1": 3, "2": 4}
r_lookup = {v: k for k, v in lookup.items()}


def snafu_to_dec(x: str):
    total = 0
    e = 1
    for i in reversed(range(len(x))):
        total += e * (s_lookup[x[i]])
        e *= 5
    return total


def conv(x: str):
    return [lookup[i] for i in x]


def add_snafu(a: str, b: str):
    print()
    print(f"input: {a=} {b=}")
    matched = list(zip_longest(reversed(conv(a)), reversed(conv(b)), fillvalue=0))
    result = []
    carry = 0
    for a1, b1 in matched:
        rem = (a1 + b1 + carry) % 5
        carry = (a1 + b1 + carry) // 5
        result.append(rem)
    if carry != 0:
        result.append(carry)

    print(matched)
    print(result)
    as_snafu = "".join(r_lookup[x] for x in reversed(result))
    print(
        f"added {snafu_to_dec(a)} and {snafu_to_dec(b)} to get {snafu_to_dec(as_snafu)} ({as_snafu})"
    )
    return as_snafu


def parse_input(input_file: str):
    return input_file.splitlines()


def main():
    add_snafu("0", "1")
    # i = parse_input(read_input("example"))
    # total = "0"
    # for n in i:
    #     total = add_snafu(total, n)
    #     print(total)


main()
