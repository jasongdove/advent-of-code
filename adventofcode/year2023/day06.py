import math
import re

from adventofcode import Day


class Day06(Day):
    def __init__(self):
        super().__init__(2023, 6)
        self.reg = re.compile("(\\d+)")

    def part01(self):
        text = super()._part01_input()
        lines = text.splitlines()
        times = list(map(int, self.reg.findall(lines[0])))
        distances = list(map(int, self.reg.findall(lines[1])))
        total = 1
        for i in range(len(times)):
            count = 0
            time = times[i]
            distance = distances[i]
            for j in range(0, time + 1):
                result = j * (time - j)
                if result > distance:
                    count += 1
            total *= count
        return total

    def part02(self):
        text = super()._part01_input()
        lines = text.splitlines()
        time = int(lines[0].split(':')[1].replace(" ", ""))
        distance = int(lines[1].split(':')[1].replace(" ", ""))
        count = 0
        for j in range(0, math.ceil((time + 1) / 2.0)):
            result = j * (time - j)
            if result > distance:
                count += 1
        return count * 2
