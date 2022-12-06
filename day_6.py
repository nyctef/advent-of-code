from pprint import pprint


input_file = """zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
"""

with open("input/6-1.txt") as f:
    input_file = f.read()

for i in range(len(input_file)):
    c = input_file[i - 14 : i]
    if len(set(c)) == 14:
        pprint(i)
        break
