from pathlib import Path
import re

input = """
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""
input = Path("./input/day24.txt").read_text()
lines = input.strip().splitlines()
input = [([int(x) for x in re.split(r"[^\d-]+", i)]) for i in lines]
# input = [i for i in input if i[0] < 100000000000000]
input = input[:6]
print(input)


from z3 import Int, Solver, Float64

rx0 = Int("rx0")
ry0 = Int("ry0")
rz0 = Int("rz0")
rdx = Int("rdx")
rdy = Int("rdy")
rdz = Int("rdz")


s = Solver()


for i, line in enumerate(input):
    hit_time = Int("t" + str(i))

    # print("line")
    # print(line)
    hx0, hy0, hz0, hdx, hdy, hdz = line

    s.add((rx0 + (rdx * hit_time)) == (hx0 + (hdx * hit_time)))
    s.add((ry0 + (rdy * hit_time)) == (hy0 + (hdy * hit_time)))
    s.add((rz0 + (rdz * hit_time)) == (hz0 + (hdz * hit_time)))
    # s.add(hit_time == (rx0 - hx0) / (hdx - rdx))
    # s.add(hit_time == (ry0 - hy0) / (hdy - rdy))
    # s.add(hit_time == (rz0 - hz0) / (hdz - rdz))
    s.add(hit_time >= 0)

print(s.check())
print(s.model())
