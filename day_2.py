from pprint import pprint


def convert(char):
    return {"A": "R", "B": "P", "C": "S", "X": "R", "Y": "P", "Z": "S"}[char]


def individual_score(strat):
    return {"R": 1, "P": 2, "S": 3}[strat]


beats = {"R": "S", "P": "R", "S": "P"}
loses = {"R": "P", "P": "S", "S": "R"}


def battle_score(opp, me):
    if opp == me:
        return 3
    if beats[me] == opp:
        return 6
    return 0


def my_strat(opp, me):
    if me == "X":
        return beats[opp]
    if me == "Y":
        return opp
    if me == "Z":
        return loses[opp]


with open("input/2-1.txt") as f:
    input_file = f.read()

#
# input_file = """A Y
# B X
# C Z
# """

lines = input_file.splitlines()
strats = []
for line in lines:
    [opp, me] = line.split()
    strats.append((convert(opp), me))
# pprint(strats)

total_score = 0
for (opp, me) in strats:
    pprint((opp, me))
    me = my_strat(opp, me)
    pprint(me)
    total_score += individual_score(me) + battle_score(opp, me)
    pprint(total_score)

print(total_score)
