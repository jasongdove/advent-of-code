from typing import Optional

import re

from adventofcode import Day


class Part:
    _x = re.compile(r'x=(\d+)')
    _m = re.compile(r'm=(\d+)')
    _a = re.compile(r'a=(\d+)')
    _s = re.compile(r's=(\d+)')

    def __init__(self, line):
        self.x = int(self._x.search(line).group(1))
        self.m = int(self._m.search(line).group(1))
        self.a = int(self._a.search(line).group(1))
        self.s = int(self._s.search(line).group(1))

    def __repr__(self):
        return f'Part(x={self.x}, m={self.m}, a={self.a}, s={self.s})'


class Rule:
    _parts = re.compile(r'(\w+)([<>])(\d+):(\w+)')

    def __init__(self, string):
        match = Rule._parts.match(string)
        (self.category, self.check, self.value, self.dest) = match.groups()
        self.value = int(self.value)

    def test(self, part: Part) -> Optional[str]:
        match self.category:
            case 'x':
                test_val = part.x
            case 'm':
                test_val = part.m
            case 'a':
                test_val = part.a
            case _:
                test_val = part.s

        match self.check:
            case '<':
                return self.dest if test_val < self.value else None
            case _:
                return self.dest if test_val > self.value else None

    def __repr__(self):
        return f'Rule({self.category}, {self.check}, {self.value})'


class Workflow:
    _name = re.compile(r'(\w+){')
    _rule = re.compile(r'(\w+[><]\d+:\w+)')
    _dest = re.compile(r',(\w+)}')

    def __init__(self, line):
        self.name = self._name.match(line).group(1)
        self.rules = []
        for rule in self._rule.findall(line):
            r = Rule(rule)
            self.rules.append(r)
            pass
        self.dest = self._dest.findall(line)[0]

    def test(self, part: Part) -> Optional[str]:
        for rule in self.rules:
            test_result = rule.test(part)
            if test_result is not None:
                return test_result
        return self.dest

    def __repr__(self):
        return f'Workflow(name={self.name}, rules={self.rules}, dest={self.dest})'


class Day19(Day):
    def __init__(self):
        super().__init__(2023, 19)

    def part01(self):
        text = super()._part01_input()
        [wfs_text, parts_text] = text.split('\n\n')
        wfs = []
        parts = []
        for line in wfs_text.splitlines():
            wf = Workflow(line)
            wfs.append(wf)
        #print(wfs)
        for line in parts_text.splitlines():
            part = Part(line)
            parts.append(part)
        #print(parts)

        accepted = []
        rejected = []
        for part in parts:
            wf = next(filter(lambda w: w.name == 'in', wfs))
            while wf:
                result = wf.test(part)
                match result:
                    case 'R':
                        rejected.append(part)
                        wf = None
                    case 'A':
                        accepted.append(part)
                        wf = None
                    case target:
                        wf = next(filter(lambda w: w.name == target, wfs))
                #print(result)

        #print(accepted)
        #print(rejected)

        return sum([a.x + a.m + a.a + a.s for a in accepted])

    def part02(self):
        text = super()._part01_input()
        return 0
