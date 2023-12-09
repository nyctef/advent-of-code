from pathlib import Path
import scipy
import numpy as np
import re

input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
input = Path("./input/day09.txt").read_text()
lines = input.strip().splitlines()
input = [np.array([int(x) for x in i.split(" ")]) for i in lines]
print([len(x) for x in input])
max = min(19, len(input[0]))
target = 21
print("max=", max)
polys = [scipy.interpolate.lagrange(range(0, max), x[:max]) for x in input]
results = [round(p(target)) for p in polys]
print("input=", input)
print("polys=", polys)
print("results=", results)
print(sum(results))
