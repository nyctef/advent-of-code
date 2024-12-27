import re
from util import get_input
import numpy as np
from numpy.typing import NDArray

marker_re = re.compile(r"\((\d+)x(\d+)\)")

def solve_for(input: str):
    input = input.strip().replace("\n", "")

    part1 = decompress(input, 0, len(input), False)
    part2 = decompress(input, 0, len(input), True)


    return (part1, part2)

def decompress(input, start, length, recurse):
    # print(f"decompress {start=} {length=}")
    ptr = start
    count = 0
    while ptr < start + length:
        if input[ptr] == "(":
            assert (marker := marker_re.search(input, ptr))
            # print("marker", input[marker.start() : marker.end()])
            (sublen, reps) = tuple(int(x) for x in marker.groups())
            subcount = decompress(input, marker.end(), sublen, recurse) if recurse else sublen
            ptr = marker.end() + sublen
            count += subcount * reps
        else:
            count += 1
            ptr += 1
    return count


if __name__ == "__main__":
    (part1, part2) = solve_for(get_input(2016, 9))
    print(f"Part 1: {part1} | Part 2: {part2}")
