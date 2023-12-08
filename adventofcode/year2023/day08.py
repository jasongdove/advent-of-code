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


class Day08(Day):
    def __init__(self):
        super().__init__(2023, 8)

    @staticmethod
    def dist(directions, m, pos):
        count = 0
        for c in itertools.cycle(directions):
            current = m[pos]
            if current.key == "ZZZ":
                break
            if c == "R":
                pos = current.right
            else:
                pos = current.left
            count += 1
        return count

    @staticmethod
    def dist2(directions, m, pos):
        count = 0
        for c in itertools.cycle(directions):
            current = m[pos]
            if current.key[2] == "Z":
                break
            if c == "R":
                pos = current.right
            else:
                pos = current.left
            count += 1
        return count

    def part01(self):
        text = super()._part01_input()
        lines = text.splitlines()
        directions = [c for c in lines[0]]
        rest = lines[2:]
        x = map(Instr, rest)
        m = {}
        for i in x:
            m[i.key] = i
        return Day08.dist(directions, m, "AAA")

    def part02(self):
        text = super()._part02_input()
        lines = text.splitlines()
        directions = [c for c in lines[0]]
        rest = lines[2:]
        x = map(Instr, rest)
        m = {}
        for i in x:
            m[i.key] = i
        pos = [k for k in m.keys() if k[2] == "A"]
        res = []
        for p in pos:
            res.append(Day08.dist2(directions, m, p))
        return math.lcm(*res)
