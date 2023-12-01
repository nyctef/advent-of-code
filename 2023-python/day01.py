from pathlib import Path
import re

input = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
input = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
input = Path("./input/day01.txt").read_text()
# input = "sevenmfpxvntvkpqvpbnnbpr5seven18sixeighteightwok"
lines = input.splitlines()


digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
digit_strings = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]

digits_re = re.compile(f"(?=({('|'.join(digits))}|{('|'.join(digit_strings))}))")

total = 0
for line in lines:
    print(line)
    if not len(line):
        continue
    first_digit = digits_re.search(line).group(1)
    try:
        first_digit = digit_strings.index(first_digit) + 1
    except ValueError:
        pass

    last_digit = digits_re.findall(line)[-1]
    try:
        last_digit = digit_strings.index(last_digit) + 1
    except ValueError:
        pass
    number = f"{first_digit}{last_digit}"
    total += int(number)

print(total)
