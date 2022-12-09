from pprint import pprint
from typing import Tuple


input_file = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""
with open("input/9-1.txt") as f:
    input_file = f.read()

instructions = input_file.splitlines()
# instructions = instructions[0:20]
instructions = [x.split() for x in instructions]
instructions = [(x, int(y)) for [x, y] in instructions]

pprint(instructions)


def distance(head, tail):
    return (head[0] - tail[0], head[1] - tail[1])


def normalize_(i: int):
    if i == 0:
        return i
    if i > 0:
        return 1
    if i < 0:
        return -1


def normalize(pos: Tuple[int, int]):
    return (normalize_(pos[0]), normalize_(pos[1]))


def invert(pos: Tuple[int, int]):
    return (-pos[0], -pos[1])


def add(pos1: Tuple[int, int], pos2: Tuple[int, int]):
    return (pos1[0] + pos2[0], pos1[1] + pos2[1])


def mag(pos: Tuple[int, int]):
    return abs(pos[0]) + abs(pos[1])


def move(pos: Tuple[int, int], direction):
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


Pos = Tuple[int, int]


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
        raise Exception("asdf")


head = (0, 0)
tail = (0, 0)
tail_positions = set()
tail_positions.add(tail)

for (direction, count) in instructions:
    print(f"\n>>> {direction=} {count=}")
    for _ in range(count):
        head = move(head, direction)
        d = distance(head, tail)
        print(f"{head=} {tail=} {d=}")
        if abs(d[0]) <= 1 and abs(d[1]) <= 1:
            continue
        tail = correct_tail(head, tail)
        tail_positions.add(tail)
        print(f"tail moves to {tail=}")

print(len(tail_positions))
