from adventofcode import Day
import re
import itertools
import math


class Instr:

    def __init__(self, line):
        reg = re.compile("([A-Z0-9]{3})\\s+=\\s+\\(([A-Z0-9]{3}),\\s+([A-Z0-9]{3})\\)")
        m = reg.match(line)
        self.key = m.groups(0)[0]
        self.left = m.groups(0)[1]
        self.right = m.groups(0)[2]


class Day09(Day):
    def __init__(self):
        super().__init__(2023, 9)

    def part01(self):
        text = super()._part01_input()
        return 0

    def part02(self):
        text = super()._part01_input()
        return 0
