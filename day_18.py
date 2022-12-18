from pathlib import Path
from typing import List, Tuple


def read_input(name: str):
    match name:
        case "simple-1":
            return """1,1,1
2,1,1
"""
        case "example":
            return """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"""
        case "puzzle":
            return Path("input/18-1.txt").read_text()

        case other:
            raise Exception(other)


def parse_input(input: str):
    return [eval(f"({line})") for line in input.splitlines()]


def m_dist(p1: Tuple[int, int, int], p2: Tuple[int, int, int]):
    return sum(abs(x - y) for x, y in zip(p1, p2))


def main():
    input_text = read_input("puzzle")
    lines = parse_input(input_text)

    count_shared_sides = 0
    for i1, p1 in enumerate(lines):
        for i2, p2 in enumerate(lines):
            if i1 == i2:
                # print(f"skipping comparing index {i1=} to itself")
                continue
            # print(f"comparing {p1=} and {p2=} [{type(p1)=} {type(p2)=}]")
            if m_dist(p1, p2) == 1:
                # print(f"{p1=} and {p2=} share a side")
                count_shared_sides += 1

    print(f"{len(lines)=} {count_shared_sides=}")
    print((len(lines) * 6) - (count_shared_sides))

    # print(f"{type((2,1,1))=}")
    # print(m_dist((2, 1, 1), (1, 1, 1)))


if __name__ == "__main__":
    main()
