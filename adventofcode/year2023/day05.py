from adventofcode import Day
import re
import math

class Day05(Day):
    def __init__(self):
        super().__init__(2023, 5)
        self.reg = re.compile("(\\d+)")

    def process(self, lines, value):
        next_value = value
        for i in range(1, len(lines)):
            line = lines[i]
            if ':' in line:
                continue
            elif line == '':
                value = next_value
                continue
            elif value is not None:
                range_map = list(map(int, line.split(' ')))
                if range_map[1] <= value < (range_map[1] + range_map[2]):
                    next_value = value - range_map[1] + range_map[0]
                    value = None
        return value or next_value

    def part01(self):
        text = super()._part01_input()
        lines = text.splitlines()
        result = list(map(int, self.reg.findall(lines[0])))
        next_result = []
        for i in range(1, len(lines)):
            line = lines[i]
            if ':' in line:
                next_result = []
                continue
            elif line == '':
                next_result.extend(result)
                result = next_result
                continue
            range_map = list(map(int, line.split(' ')))
            for j in result[:]:
                if range_map[1] <= j <= (range_map[1] + range_map[2]):
                    value = j - range_map[1] + range_map[0]
                    next_result.append(value)
                    result.remove(j)
        next_result.extend(result)
        result = next_result
        return min(result)

    def part02(self):
        text = super()._part01_input()
        lines = text.splitlines()
        result = list(map(int, self.reg.findall(lines[0])))
        best_seed = None
        seed_step = None
        min_result = 10000000000000000000
        # for each seed range
        for r in range(0, len(result), 2):
            lo = result[r]
            hi = lo + result[r + 1] - 1
            # check a handful of seeds using large steps, to find the best seed range
            step = max(1, int(math.ceil(hi - lo) / 10_000.0))
            for t in range(lo, hi + 1, step):
                res = self.process(lines, t)
                if res < min_result:
                    min_result = res
                    best_seed = t
                    seed_step = step

        # check every number around that specific seed (+/- step)
        min_result = 10000000000000000000
        for i in range(best_seed - seed_step, best_seed + seed_step + 1):
            res = self.process(lines, i)
            min_result = min(min_result, res)
        return min_result
