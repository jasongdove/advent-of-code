from __future__ import annotations
from typing import Optional

from collections import deque
import re

from adventofcode import Day


class PartRange:
    def __init__(self, x=(1, 4000), m=(1, 4000), a=(1, 4000), s=(1, 4000)):
        self.x = x
        self.m = m
        self.a = a
        self.s = s

    def branch(self, rule) -> list[(PartRange, str)]:
        result = []
        match (rule.category, rule.check):
            case ('x', '>'):
                result.append((PartRange((self.x[0], min(self.x[1], rule.value)), self.m, self.a, self.s), None))
                result.append((PartRange((max(self.x[0], rule.value + 1), self.x[1]), self.m, self.a, self.s), rule.dest))
            case ('x', '<'):
                result.append((PartRange((self.x[0], min(self.x[1], rule.value - 1)), self.m, self.a, self.s), rule.dest))
                result.append((PartRange((max(self.x[0], rule.value), self.x[1]), self.m, self.a, self.s), None))
            case ('m', '>'):
                result.append((PartRange(self.x, (self.m[0], min(self.m[1], rule.value)), self.a, self.s), None))
                result.append((PartRange(self.x, (max(self.m[0], rule.value + 1), self.m[1]), self.a, self.s), rule.dest))
            case ('m', '<'):
                result.append((PartRange(self.x, (self.m[0], min(self.m[1], rule.value - 1)), self.a, self.s), rule.dest))
                result.append((PartRange(self.x, (max(self.m[0], rule.value), self.m[1]), self.a, self.s), None))
            case ('a', '>'):
                result.append((PartRange(self.x, self.m, (self.a[0], min(self.a[1], rule.value)), self.s), None))
                result.append((PartRange(self.x, self.m, (max(self.a[0], rule.value + 1), self.a[1]), self.s), rule.dest))
            case ('a', '<'):
                result.append((PartRange(self.x, self.m, (self.a[0], min(self.a[1], rule.value - 1)), self.s), rule.dest))
                result.append((PartRange(self.x, self.m, (max(self.a[0], rule.value), self.a[1]), self.s), None))
            case ('s', '>'):
                result.append((PartRange(self.x, self.m, self.a, (self.s[0], min(self.s[1], rule.value))), None))
                result.append((PartRange(self.x, self.m, self.a, (max(self.s[0], rule.value + 1), self.s[1])), rule.dest))
            case ('s', '<'):
                result.append((PartRange(self.x, self.m, self.a, (self.s[0], min(self.s[1], rule.value - 1))), rule.dest))
                result.append((PartRange(self.x, self.m, self.a, (max(self.s[0], rule.value), self.s[1])), None))
        return result

    def combinations(self) -> int:
        return (self.x[1] - (self.x[0] - 1)) * (self.m[1] - (self.m[0] - 1)) * (self.a[1] - (self.a[0] - 1)) * (self.s[1] - (self.s[0] - 1))

    def __repr__(self):
        return f'PartRange(x={self.x}, m={self.m}, a={self.a}, s={self.s})'


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
        return f'Rule({self.category}, {self.check}, {self.value}, {self.dest})'


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

    def branch(self, part: PartRange, workflows: dict[str, Workflow]) -> list[(PartRange, Optional[Workflow])]:
        result = []
        fail_q = deque([part])
        for rule in self.rules:
            next_q = deque()
            while len(fail_q) > 0:
                pt = fail_q.popleft()
                branches = pt.branch(rule)
                for p, wf in branches:
                    if wf is not None:
                        if wf == 'R':
                            pass
                        elif wf == 'A':
                            result.append((p, None))
                        else:
                            result.append((p, workflows[wf]))
                    else:
                        next_q.append(p)
            fail_q = next_q
        if self.dest == 'A':
            result.extend(map(lambda p2: (p2, None), fail_q))
        elif self.dest != 'R':
            result.extend(map(lambda p2: (p2, workflows[self.dest]), fail_q))
        return result

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
        wfs = {}
        parts = []

        for line in wfs_text.splitlines():
            wf = Workflow(line)
            wfs[wf.name] = wf

        for line in parts_text.splitlines():
            part = Part(line)
            parts.append(part)

        accepted = []
        rejected = []
        for part in parts:
            wf = wfs['in']
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
                        wf = wfs[target]

        return sum([a.x + a.m + a.a + a.s for a in accepted])

    def part02(self):
        text = super()._part02_input()
        [wfs_text, _] = text.split('\n\n')
        wfs = {}

        for line in wfs_text.splitlines():
            wf = Workflow(line)
            wfs[wf.name] = wf

        accepted = []
        wf = wfs['in']
        parts = [(PartRange(), wf)]
        while len(parts) > 0:
            next_parts = []
            for _ in range(len(parts)):
                (part, wf) = parts.pop()
                branches = wf.branch(part, wfs)
                for branch in branches:
                    if branch[1] is None:
                        accepted.append(branch[0])
                    else:
                        next_parts.append(branch)
            parts = next_parts

        return sum(map(lambda a: a.combinations(), accepted))
