from adventofcode import Day
from functools import cache


class Day12(Day):
    def __init__(self):
        super().__init__(2023, 12)
        self.memo = dict()

    def solve_line(self, line: str, unfold: int) -> int:
        self.memo.clear()
        s = line.split()
        r = s[0]
        c = list(map(int, s[1].split(',')))
        records = tuple()
        contig = tuple()
        for i in range(0, unfold):
            records = records + tuple(r)
            records = records + tuple(['?'])
            contig = contig + tuple(c)
        records = records[:len(records)-1]
        result = self.solve(records, contig)
        return result

    @cache
    def solve(self, records: tuple[chr], contig: tuple[int]) -> int:
        result = 0
        r = list(records)
        c = list(contig)
        if len(r) == 0:
            match len(c):
                case 0:
                    result = 1
                case 1:
                    result = 1 if c[0] == 0 else 0
                case _:
                    result = 0
        else:
            head = r.pop(0)
            match head:
                case '?':
                    one = self.solve(tuple(['#'] + r), tuple(c))
                    two = self.solve(tuple(['.'] + r), tuple(c))
                    result = one + two
                case '#':
                    if len(contig) == 0:
                        result = 0
                    else:
                        head_contig = c.pop(0)
                        head_contig -= 1
                        while head_contig > 0 and len(r) > 0 and r[0] in ['#', '?']:
                            r.pop(0)
                            head_contig -= 1
                        if len(r) == 0:
                            result = 1 if head_contig == 0 and len(c) == 0 else 0
                        elif head_contig > 0:
                            if r[0] == '.':
                                result = 0
                            else:
                                result = self.solve(tuple(r), tuple(c))
                        elif head_contig == 0:
                            if r[0] == '?':
                                r[0] = '.'
                            if r[0] == '.':
                                result = self.solve(tuple(r), tuple(c))
                            else:
                                result = 0
                        else:
                            result = 0
                case '.':
                    if len(c) > 0:
                        result = self.solve(tuple(r), tuple(c))
                    else:
                        result = all(x != '#' for x in r)
        return result

    def part01(self):
        text = super()._part01_input()
        arrangements = [self.solve_line(x, 1) for x in text.splitlines()]
        return sum(arrangements)

    def part02(self):
        text = super()._part01_input()
        arrangements = [self.solve_line(x, 5) for x in text.splitlines()]
        return sum(arrangements)
