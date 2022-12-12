from multiprocessing import Pool
from pathlib import Path
from pprint import pprint
from typing import Dict, List, Tuple


input_file = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""

input_file = Path("input/12-1.txt").read_text()

input_file = input_file.replace("S", "`").replace("E", "{")
lines = input_file.splitlines()
width = len(lines[0])
height = len(lines)
elevations = [[-1] * width for _ in range(height)]
for (r, line) in enumerate(lines):
    for (c, chr) in enumerate(line):
        elevations[r][c] = ord(chr) - ord("a")

Coord = Tuple[int, int]
starts: List[Coord] = []
end = (-1, -1)

for (r, column) in enumerate(elevations):
    for (c, x) in enumerate(column):
        if x == 0:
            starts.append((r, c))
        elif x == 26:
            end = (r, c)

pprint(elevations)
print(f"{starts=} {end=}")


def enumerate_elevations():
    for (r, line) in enumerate(elevations):
        for (c, x) in enumerate(line):
            yield ((r, c), x)


def addc(a: Coord, b: Coord):
    return (a[0] + b[0], a[1] + b[1])


can_move_from_to: Dict[Coord, List[Coord]] = {}

offsets = [(-1, 0), (+1, 0), (0, -1), (0, +1)]

for (coord, elevation) in enumerate_elevations():
    valid_targets: List[Coord] = []
    for offset in offsets:
        target = addc(coord, offset)
        if target[0] < 0 or target[1] < 0 or target[0] >= height or target[1] >= width:
            continue
        target_elevation = elevations[target[0]][target[1]]
        if target_elevation <= elevation + 1:
            valid_targets.append(target)
    can_move_from_to[coord] = valid_targets

# pprint(can_move_from_to)


# let's have a go at implementing dijkstra


def dijkstra(start: Coord):
    dist: Dict[Coord, int] = {}
    prev: Dict[Coord, Coord] = {}

    queue: List[Coord] = []

    for (coord, elevation) in enumerate_elevations():
        dist[coord] = 99999999999999
        queue.append(coord)

    dist[start] = 0

    while queue:
        # slow - should try a prioritized queue at some point
        queue = list(sorted(queue, key=lambda x: dist[x], reverse=True))
        u = queue.pop()

        for v in queue:
            if not v in can_move_from_to[u]:
                continue
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u
    return dist, prev


def run_dijkstra(start):
    (dist, prev) = dijkstra(start)
    return dist[end]


# (dist, prev) = dijkstra((20, 0))

# sorted_starts = sorted(starts, key=lambda x: dist[x], reverse=True)
# print((sorted_starts[0], dist[end] - dist[sorted_starts[0]]))

# with Pool() as p:
#     results = p.map(run_dijkstra, starts[:10])
# print(sorted(results)[:10])

# results = []
# for start in starts:
#     result = run_dijkstra(start)
#     results.append((start, result))

# print(results)
# print(sorted(results)[0])

results = []
(dist, prev) = dijkstra(end)
for start in starts:
    results.append(dist[start])
print(sorted(results)[0])
