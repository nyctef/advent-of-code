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
    log(f"moving {to_move[1]} from index {current_i}")
    move_distance = to_move[1]
    s = sign(move_distance)
    if move_distance == 0:
        log(f"zero doesn't move")
        return
    if abs(move_distance) % (len(parsed) - 1) == 0:
        log(f"moving n-1 positions seems to be a loop")
        return
    if abs(move_distance) >= len(parsed):
        # ????
        move_distance += sign(move_distance)
    if abs(move_distance) >= len(parsed) * 2:
        # ????
        move_distance += sign(move_distance)
    bound_i1 = (current_i + move_distance) % len(parsed)
    bound_i2 = (current_i + move_distance + sign(move_distance)) % len(parsed)
    bound_i1, bound_i2 = min(bound_i1, bound_i2), max(bound_i1, bound_i2)
    if bound_i1 == current_i:
        bound_i1 = (bound_i1 - 1) % len(parsed)
    if bound_i2 == current_i:
        bound_i2 = (bound_i2 + 1) % len(parsed)
    bound_1 = parsed[bound_i1][1]
    bound_2 = parsed[bound_i2][1]
    log(f"{to_move[1]} moves between {bound_1} and {bound_2}")
    log("removing to_move")
    # log("finding where our bounds ended up")
    # bound_i1, _ = next((i, e) for i, e in enumerate(parsed) if e[1] == bound_1)
    # bound_i2, _ = next((i, e) for i, e in enumerate(parsed) if e[1] == bound_2)

    min_index = min(bound_i1, bound_i2)
    max_index = max(bound_i1, bound_i2)
    log(f"{bound_i1=} {bound_i2=} {min_index=} {max_index=}")
    if min_index == 0 and max_index == len(parsed) - 1:
        log(f"sticking it at the end of the list")
        insert_location = len(parsed)
    else:
        insert_location = max_index

    parsed.insert(insert_location, to_move)
    log(f"after inserting: {parsed}")
    if insert_location <= current_i:
        current_i += 1
    del parsed[current_i]


def main(input_name, log):
    input_file = read_input(input_name)
    just_nums = [int(x) for x in input_file.splitlines()]
    parsed = list(enumerate(just_nums))
    # assert len(just_nums) == len(set(just_nums))
    log([x[1] for x in parsed])
    print(len(parsed))

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
        l = list(enumerate([x if a == "x" else a for a in l1 if a != x]))
        print(f" testing with {x=}")
        print([x[1] for x in l])
        step_index = next(i for i, e in enumerate(l) if e[1] == x)
        do_mix_step(step_index, l, print if x in (5,) else nothing)
        print([x[1] for x in l])
        print()


def test_2():
    print(f"test 2 ----------------")
    l1 = [0, 1, 2, 3, "x", 5, 6, 7, 8, 9]
    for x in range(-24, 0):
        # for x in [-9, -2]:
        l = list(enumerate([x if a == "x" else a for a in l1 if a != x]))
        print(f" testing with {x=}")
        print([x[1] for x in l])
        do_mix_step(4, l, print if x in (-10,) else nothing)
        print([x[1] for x in l])
        print()


if __name__ == "__main__":
    main("example", print)
    main("puzzle", nothing)
    # test_1()
    # test_2()
