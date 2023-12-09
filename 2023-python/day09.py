from pathlib import Path
import re


def find_layers(nums: list[int]) -> list[list[int]]:
    nums = nums.copy()
    result = [nums]
    while True:
        next_layer = []
        for i in range(1, len(nums)):
            next_layer.append(nums[i] - nums[i - 1])
        if all(x == 0 for x in next_layer):
            break
        result.append(next_layer)
        nums = result[-1].copy()
    return result


def incr_layers(ls: list[list[int]]):
    ls = ls.copy()
    res = 0
    for i in range(len(ls) - 2, -1, -1):
        ls[i].append(ls[i][-1] + ls[i + 1][-1])
        res = ls[i][-1]
    return res


input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
input = Path("./input/day09.txt").read_text()
lines = input.strip().splitlines()
input = [([int(x) for x in i.split(" ")]) for i in lines]

target = 5

total = 0

for l in input:
    layers = find_layers(l)
    total += incr_layers(layers)

print("total=", total)

exit()
# print([len(x) for x in input])
max = min(19, len(input[0]))
# print("max=", max)
polys = [scipy.interpolate.lagrange(range(0, max), x[:max]) for x in input]
results = [round(p(target)) for p in polys]
print("input=", input)
print("polys=", polys)
print("results=", results)
print(sum(results))
