from adventofcode import Day


class Day15(Day):
    def __init__(self):
        super().__init__(2023, 15)

    def part01(self):
        text = super()._part01_input()
        steps = text.replace('\n', '').split(',')
        total = 0
        for step in steps:
            current = 0
            for _, c in enumerate(step):
                current += ord(c)
                current *= 17
                current %= 256
            print(f'{step} => {current}')
            total += current
        return total

    def part02(self):
        text = super()._part01_input()
        return 0
