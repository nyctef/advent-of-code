from pathlib import Path
import scipy
import numpy as np
import re
import lagrange_polynomial


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


input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
# input = Path("./input/day09.txt").read_text()
lines = input.strip().splitlines()
input = [([int(x) for x in i.split(" ")]) for i in lines]

target = 5

total = 0

for l in input:
    degree = len(find_layers(l)) - 1
    print("l=", l)
    print("degree=", degree)
    xs = range(0, degree + 1)
    ys = l[: degree + 1]
    print("xs,ys=", xs, ys)
    poly = lagrange_polynomial.LagrangePolynomial(xs, ys)
    # print("degree=", degree)
    # print(poly)
    print(poly(target))
    total += poly(target)
    print("min_error=", "{:f}".format(round(poly(target)) - poly(target)))

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
