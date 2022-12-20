from pprint import pprint
from typing import List, Set, Tuple


input_file = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""

input_file = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""

with open("input/9-1.txt") as f:
    input_file = f.read()

instructions = input_file.splitlines()
# instructions = instructions[0:20]
instructions = [x.split() for x in instructions]
instructions = [(x, int(y)) for [x, y] in instructions]

pprint(instructions)


Pos = Tuple[int, int]


def distance(head, tail):
    return (head[0] - tail[0], head[1] - tail[1])


def normalize_(i: int):
    if i > 0:
        return 1
    if i < 0:
        return -1
    return 0


def normalize(pos: Pos):
    return (normalize_(pos[0]), normalize_(pos[1]))


def invert(pos: Pos):
    return (-pos[0], -pos[1])


def add(pos1: Pos, pos2: Pos):
    return (pos1[0] + pos2[0], pos1[1] + pos2[1])


def mag(pos: Pos):
    return abs(pos[0]) + abs(pos[1])


def move(pos: Pos, direction):
    if direction == "R":
        return add(pos, (1, 0))
    if direction == "U":
        return add(pos, (0, 1))
    if direction == "L":
        return add(pos, (-1, 0))
    if direction == "D":
        return add(pos, (0, -1))
    else:
        raise Exception("asdf")


def correct_tail(head: Pos, tail: Pos):
    d = distance(head, tail)
    if abs(d[0]) > abs(d[1]):
        # long axis is horizontal: tail should be placed to the side of head
        d_ = normalize_(d[0])
        print(f"horiz {d=} {d_=} {tail=} {head=}")
        return (head[0] - d_, head[1])
    elif abs(d[1]) > abs(d[0]):
        # long axis is vertical: tail should be placed above or below
        d_ = normalize_(d[1])
        return (head[0], head[1] - d_)
    else:
        # exactly diagonal
        d_ = normalize(d)
        return (head[0] - d_[0], head[1] - d_[1])
        # raise Exception(f"correct_tail {head=} {tail=} {d}")


rope: List[Pos] = [(0, 0) for _ in range(10)]
tail_positions: Set[Pos] = set()
tail_positions.add((0, 0))

for (direction, count) in instructions:
    print(f"\n>>> {direction=} {count=}")
    for _ in range(count):
        print()
        rope[0] = move(rope[0], direction)
        print(f"{rope[0]=}")
        for knot in range(1, 10):
            print(f"{knot=}")
            head = rope[knot - 1]
            tail = rope[knot]
            d = distance(head, tail)
            print(f"{head=} {tail=} {d=}")
            if abs(d[0]) <= 1 and abs(d[1]) <= 1:
                continue
            tail = correct_tail(head, tail)
            print(f"{knot=} moves to {tail=}")
            rope[knot] = tail
            if knot == 9:
                tail_positions.add(tail)
    print(f"\n>>> {rope=}")

print(len(tail_positions))
