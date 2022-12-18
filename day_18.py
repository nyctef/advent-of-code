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


def add(p1: Tuple[int, int, int], p2: Tuple[int, int, int]):
    return tuple(a + b for a, b in zip(p1, p2))


def main():
    input_text = read_input("puzzle")
    droplet_points: set[Tuple[int, int, int]] = set(parse_input(input_text))

    # add 1 so we can start at 0,0,0 and have space for the steam to move around
    droplet_points = set((x + 1, y + 1, z + 1) for (x, y, z) in droplet_points)

    # idea:
    # find the min/max limits of the points sets, then set bounds of that +/- 1
    # start with a steam point that's outside the shape
    # dfs to find all points within the bounds but not crossing shape boundaries
    # steam can only move in 6 cardinal directions
    #    ~can say 3 directions so long as we know which corner we're in~
    #    actually no since we have to move around the boulder
    # once we have a set of steam points and a set of droplet points, count
    # all cases where steam points are touching droplet points for the answer

    min_x = min(p[0] for p in droplet_points)
    max_x = max(p[0] for p in droplet_points)
    min_y = min(p[1] for p in droplet_points)
    max_y = max(p[1] for p in droplet_points)
    min_z = min(p[2] for p in droplet_points)
    max_z = max(p[2] for p in droplet_points)

    print(f"{min_x=} {max_x=} {min_y=} {max_y=} {min_z=} {max_z=}")

    start = (0, 0, 0)
    bounds = (max_x + 1, max_y + 1, max_z + 1)

    print(f"total possible area = {bounds[0] * bounds[1] * bounds[2]}")
    seen: set[Tuple[int, int, int]] = set()
    queue: List[Tuple[int, int, int]] = []
    queue.append(start)

    steam_points: set[Tuple[int, int, int]] = set()

    while queue:
        next = queue.pop()
        seen.add(next)
        if next in droplet_points:
            continue
        else:
            steam_points.add(next)

        for direction in [
            (1, 0, 0),
            (-1, 0, 0),
            (0, 1, 0),
            (0, -1, 0),
            (0, 0, 1),
            (0, 0, -1),
        ]:
            c = add(next, direction)
            if c[0] < start[0] or c[1] < start[1] or c[2] < start[2]:
                continue
            if c[0] > bounds[0] or c[1] > bounds[1] or c[2] > bounds[2]:
                continue
            if c in seen:
                continue
            queue.append(c)

    print(f"{len(droplet_points)=}")
    print(f"{len(steam_points)=}")

    outside_surface = 0
    for d in droplet_points:
        for s in steam_points:
            if m_dist(d, s) == 1:
                # air/rock boundary, so count it
                outside_surface += 1

    print(f"{outside_surface=}")


if __name__ == "__main__":
    main()
