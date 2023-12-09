from adventofcode import Day
import re


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
        self.reg = re.compile("(-?\\d+)")

    @staticmethod
    def get_next_item(line):
        history = [line]
        while True:
            last_line = history[-1]
            if all(x == 0 for x in last_line):
                break
            next_line = []
            for i in range(1, len(last_line)):
                next_line.append(last_line[i] - last_line[i - 1])
            history.append(next_line)
        reverse = list(reversed(history))
        reverse[0].append(0)
        for i in range(1, len(reverse)):
            next_item = (reverse[i - 1][-1] + reverse[i][-1])
            reverse[i].append(next_item)
        return reverse[-1][-1]

    @staticmethod
    def get_prev_item(line):
        history = [line]
        while True:
            last_line = history[-1]
            if all(x == 0 for x in last_line):
                break
            next_line = []
            for i in range(1, len(last_line)):
                next_line.append(last_line[i] - last_line[i - 1])
            history.append(next_line)
        reverse = list(reversed(history))
        reverse[0].append(0)
        for i in range(1, len(reverse)):
            next_item = (reverse[i - 1][-1] + reverse[i][-1])
            reverse[i].append(next_item)
        print(reverse)
        return reverse[-1][-1]

    def part01(self):
        text = super()._part01_input()
        numeric_lines = []
        for line in text.splitlines():
            numeric_lines.append(list(map(int, self.reg.findall(line))))
        next_items = map(Day09.get_next_item, numeric_lines)
        return sum(next_items)

    def part02(self):
        text = super()._part02_input()
        numeric_lines = []
        for line in text.splitlines():
            numeric_lines.append(list(map(int, self.reg.findall(line))))
        next_items = map(Day09.get_prev_item, numeric_lines)
        return sum(next_items)
