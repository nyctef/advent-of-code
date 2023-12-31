from z3 import *
import re
from functools import reduce

input = """
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
""".strip()


digits_re = re.compile(r"-?\d+")
ingredients = [digits_re.findall(l) for l in input.splitlines()]

opt = Optimize()

vars = []
exprs = []
cap = 0
dur = 0
fla = 0
tex = 0
for (i, ingr) in enumerate(ingredients):
    ingr_var = Int("i" + str(i))
    vars.append(ingr_var)
    opt.add(ingr_var > 0)
    opt.add(ingr_var < 100)

    cap += (ingr_var * ingr[0])
    dur += (ingr_var * ingr[1])
    fla += (ingr_var * ingr[2])
    tex += (ingr_var * ingr[3])

ingr_count = reduce(lambda a,b: a+b, vars, 0)
opt.add(ingr_count == 100)

cap = If(cap < 0, 0, cap)
dur = If(dur < 0, 0, dur)
fla = If(fla < 0, 0, fla)
tex = If(tex < 0, 0, tex)

score = Int('score')
score = cap * dur * fla * tex
max_score = opt.maximize(score)

print(opt.check())
print(opt.model())
print(max_score.value().as_long())

