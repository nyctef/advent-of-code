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


def main():
    input_file = read_input("example")
    parsed = [int(x) for x in input_file.splitlines()]
    parsed = list(enumerate(parsed))

    for step in range(len(parsed)):
        # print(f"{step=}")
        i, to_move = next(((i, x) for i, x in enumerate(parsed) if x[0] == step))
        # print(f"moving {to_move[1]} from index {i}")
        where_to_move = to_move[1]
        # off-by-one hacks
        if i + where_to_move <= 0:
            where_to_move -= 1
        if i + where_to_move >= len(parsed):
            where_to_move += 1
        direction = sign(where_to_move)
        # print(f"moving {to_move[1]} {where_to_move=} {direction=}")
        current_i = i
        to_move_to = (i + where_to_move) % len(parsed)

        del parsed[current_i]
        parsed.insert(to_move_to, to_move)

        # pprint(parsed)
        # pprint([x[1] for x in parsed])
        # print()

    print([x[1] for x in parsed])
    zero_index, zero = next((i, x) for i, x in enumerate(parsed) if x[1] == 0)
    x = parsed[(zero_index + 1000) % len(parsed)][1]
    y = parsed[(zero_index + 2000) % len(parsed)][1]
    z = parsed[(zero_index + 3000) % len(parsed)][1]
    print(f"{zero_index=} {x=} {y=} {z=}")
    print(x + y + z)


if __name__ == "__main__":
    main()
