from pathlib import Path
from pprint import pprint
from typing import Any


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


def nothing(*args):
    pass


def do_mix_step(step: int, parsed: list, log: Any):
    log(f"step {step+1}")
    current_i, to_move = next(((i, x) for i, x in enumerate(parsed) if x[0] == step))

    distance = to_move[1]
    del parsed[current_i]
    new_loc = (current_i + distance) % len(parsed)
    parsed.insert(new_loc, to_move)


def main(input_name, log, multiplier, rounds):
    input_file = read_input(input_name)
    just_nums = [int(x) for x in input_file.splitlines()]
    just_nums = [x * multiplier for x in just_nums]
    parsed = list(enumerate(just_nums))
    # assert len(just_nums) == len(set(just_nums))
    log([x[1] for x in parsed])
    print(len(parsed))

    for round in range(rounds):
        print()
        print(f"round {round+1}")
        print("-----------------------------------------")
        for step in range(len(parsed)):
            do_mix_step(step, parsed, log)
            if len(parsed) < 20:
                log([x[1] for x in parsed])

    if len(parsed) < 20:
        print([x[1] for x in parsed])
    zero_index, zero = next((i, x) for i, x in enumerate(parsed) if x[1] == 0)
    x = parsed[(zero_index + 1000) % len(parsed)][1]
    y = parsed[(zero_index + 2000) % len(parsed)][1]
    z = parsed[(zero_index + 3000) % len(parsed)][1]
    print(f"{zero_index=} {x=} {y=} {z=}")
    print(x + y + z)


def test_1():
    print(f"test 1 ----------------")
    l1 = [0, 1, 2, 3, "x", 5, 6, 7, 8, 9]
    for x in range(24):
        # for x in [2, 6]:
        l = list(enumerate([x if a == "x" else a for a in l1]))
        print(f" testing with {x=}")
        print([x[1] for x in l])
        do_mix_step(4, l, print if x in (5,) else nothing)
        print([x[1] for x in l])
        print()


def test_2():
    print(f"test 2 ----------------")
    l1 = [0, 1, 2, 3, "x", 5, 6, 7, 8, 9]
    for x in range(-24, 0):
        # for x in [-9, -2]:
        l = list(enumerate([x if a == "x" else a for a in l1]))
        print(f" testing with {x=}")
        print([x[1] for x in l])
        do_mix_step(4, l, print if x in (-10,) else nothing)
        print([x[1] for x in l])
        print()


if __name__ == "__main__":
    # main("example", print, 1, 1)
    # main("example", print, 811589153, 10)
    main("puzzle", nothing, 811589153, 10)
    # test_1()
    # test_2()
