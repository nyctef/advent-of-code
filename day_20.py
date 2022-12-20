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


def nothing(*args):
    pass


log = nothing


def do_mix_step(step: int, parsed: list):
    log(f"step {step+1}")
    i, to_move = next(((i, x) for i, x in enumerate(parsed) if x[0] == step))
    log(f"moving {to_move[1]} from index {i}")
    where_to_move = to_move[1]

    if where_to_move == 0:
        print(f"{step=} {to_move=} ignoring zero")
        return
    if where_to_move % len(parsed) == 0:
        print(f"{step=} {to_move=} ignoring loop?")
        return
    to_move_to = (i + where_to_move) % len(parsed)
    if abs(to_move_to) < 5:
        print(
            f"{step=} near-boundary condition moving {to_move} at {i} to {to_move_to}"
        )
    target_item = parsed[to_move_to]
    log(f"{to_move[1]=} wants to end up where {target_item[1]} currently is")

    log(f"we remove the item we're moving first")
    parsed.remove(to_move)
    log(f"then find where our target item has ended up")
    try:
        new_i = next(i for i, e in enumerate(parsed) if e == target_item)
    except StopIteration:
        print(f"{step=} failed to find {target_item} in list")
        raise
    log(f"our target item ended up at index {new_i}")
    # list.insert takes the index of the item to be ahead of,
    # so since positive moves want to go after their target item
    # we need to add one here to be inserted before the next element
    insert_fixup = 1 if where_to_move > 0 else 0
    log(f"so we're going to be at {insert_fixup=} -> {new_i + insert_fixup=}")
    updated_loc = new_i + insert_fixup
    if where_to_move < 0 and updated_loc == 0:
        # if we're trying to replace the item at index zero, that item stays where it is:
        # we end up looping to the end of the list
        updated_loc = len(parsed)
    # if updated_loc == len(parsed):
    #     # guess: maybe the opposite is true as well?
    #     # TODO: this one hasn't been proven
    #     updated_loc = 0

    # if abs(where_to_move) >= len(parsed):
    #     print(
    #         f"{step=} moving {to_move=} from current index {i} to {to_move_to=} replacing {target_item=} {direction=} {updated_loc=}"
    #     )
    parsed.insert(updated_loc, to_move)

    # print(parsed)
    log([x[1] for x in parsed])
    log()


def main(input_name):
    input_file = read_input(input_name)
    parsed = [int(x) for x in input_file.splitlines()]
    parsed = list(enumerate(parsed))
    log([x[1] for x in parsed])
    print(len(parsed))

    for step in range(len(parsed)):
        do_mix_step(step, parsed)

    if len(parsed) < 20:
        print([x[1] for x in parsed])
    zero_index, zero = next((i, x) for i, x in enumerate(parsed) if x[1] == 0)
    x = parsed[(zero_index + 1000) % len(parsed)][1]
    y = parsed[(zero_index + 2000) % len(parsed)][1]
    z = parsed[(zero_index + 3000) % len(parsed)][1]
    print(f"{zero_index=} {x=} {y=} {z=}")
    print(x + y + z)


def tests():
    print(f"test 1")
    l1 = [0, 1, 2, 3, "x", 5, 6, 7, 8, 9]
    for x in range(12):
        l = list(enumerate([x if a == "x" else a for a in l1]))
        print(f" testing with {x=}")
        print([x[1] for x in l])
        do_mix_step(4, l)
        print([x[1] for x in l])
        print()


if __name__ == "__main__":
    main("example")
    # main("puzzle")
    tests()
