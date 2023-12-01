from adventofcode import Day


class Day01(Day):
    def __init__(self):
        super().__init__(2023, 1)

    def part01(self):
        text = super()._part01_input()
        total = 0
        for line in text.splitlines():
            first = next(x for x in line if x.isdigit())
            last = next(x for x in reversed(line) if x.isdigit())
            total += int(first + last)
        return total

    @staticmethod
    def p2find(line, r):
        nums = ["_", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                "_", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        for c in r:
            for n in nums:
                if line[c:].startswith(n):
                    return str(nums.index(n) % 10)

    def part02(self):
        text = super()._part01_input()
        total = 0
        for line in text.splitlines():
            first = Day01.p2find(line, range(0, len(line)))
            last = Day01.p2find(line, range(len(line), -1, -1))
            total += int(first + last)
        return total
