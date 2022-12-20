from pathlib import Path
from pprint import pprint


def read_input(name: str):
    match name:
        case "example":
            return """1
2
-3
3
-2
0
4
"""
        case "puzzle":
            return Path("input/20-1.txt").read_text()

        case other:
            raise Exception(other)


def sign(x: int):
    return (x > 0) - (x < 0)


def swap(l: list, i1: int, i2: int):
    l[i2], l[i1] = l[i1], l[i2]


def main():
    input_file = read_input("example")
    parsed = [int(x) for x in input_file.splitlines()]
    parsed = list(enumerate(parsed))

    for step in range(len(parsed)):
        print(f"{step=}")
        i, to_move = next(((i, x) for i, x in enumerate(parsed) if x[0] == step))
        print(f"moving {to_move[1]} from index {i}")
        where_to_move = to_move[1]
        direction = sign(where_to_move)
        print(f"moving {to_move[1]} {where_to_move=} {direction=}")
        current = i
        n = (i + direction) % len(parsed)
        for m in range(abs(where_to_move)):
            print(f"swapping {current=} and {n=}")
            swap(parsed, current, n)
            current = n
            n = (n + direction) % len(parsed)
        pprint(parsed)


if __name__ == "__main__":
    main()
