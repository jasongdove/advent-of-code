from adventofcode import Day
import re
import collections
import itertools

class Instr:

    def __init__(self, line):
        reg = re.compile("([A-Z]{3})\\s+=\\s+\\(([A-Z]{3}),\\s+([A-Z]{3})\\)")
        m = reg.match(line)
        self.key = m.groups(0)[0]
        self.left = m.groups(0)[1]
        self.right = m.groups(0)[2]
        print(f'{self.key} => {self.left}, {self.right}')


class Day08(Day):
    def __init__(self):
        super().__init__(2023, 8)

    def part01(self):
        text = super()._part01_input()
        lines = text.splitlines()
        directions = [c for c in lines[0]]
        rest = lines[2:]
        x = map(Instr, rest)
        m = {}
        for i in x:
            m[i.key] = i
        pos = "AAA"
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

    def part02(self):
        text = super()._part01_input()
        return 0
