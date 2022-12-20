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


def main():
    input_file = read_input("puzzle")
    parsed = [int(x) for x in input_file.splitlines()]
    parsed = list(enumerate(parsed))
    log([x[1] for x in parsed])

    for step in range(len(parsed)):
        log(f"step {step+1}")
        i, to_move = next(((i, x) for i, x in enumerate(parsed) if x[0] == step))
        log(f"moving {to_move[1]} from index {i}")
        where_to_move = to_move[1]

        if where_to_move == 0:
            log(f"ignoring zero")
            continue
        to_move_to = (i + where_to_move) % len(parsed)
        target_item = parsed[to_move_to]
        log(f"{to_move[1]=} wants to end up where {target_item[1]} currently is")
        direction = sign(where_to_move)
        # log(f"moving {to_move[1]} {where_to_move=} {direction=}")
        current_i = i

        # del parsed[current_i]
        log(f"we remove the item we're moving first")
        parsed.remove(to_move)
        log(f"then find where our target item has ended up")
        try:
            new_i = next(i for i, e in enumerate(parsed) if e == target_item)
        except StopIteration:
            print(f"{step=} failed to find {target_item} in list")
            raise
        log(f"our new item ended up at index {new_i}")
        direction = 1 if direction > 0 else 0
        log(f"so we're going to be at {direction=} -> {new_i + direction=}")
        updated_loc = new_i + direction
        if updated_loc == 0:
            updated_loc = len(parsed)
        parsed.insert(updated_loc, to_move)

        # print(parsed)
        log([x[1] for x in parsed])
        log()

    log([x[1] for x in parsed])
    zero_index, zero = next((i, x) for i, x in enumerate(parsed) if x[1] == 0)
    x = parsed[(zero_index + 1000) % len(parsed)][1]
    y = parsed[(zero_index + 2000) % len(parsed)][1]
    z = parsed[(zero_index + 3000) % len(parsed)][1]
    print(f"{zero_index=} {x=} {y=} {z=}")
    print(x + y + z)


if __name__ == "__main__":
    main()
