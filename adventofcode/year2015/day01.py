from adventofcode import Day


class Day01(Day):
    def __init__(self):
        super().__init__(2015, 1)

    def part01(self):
        text = super()._part01_input()
        floor = 0
        for c in text:
            if c == '(':
                floor += 1
            else:
                floor -= 1
        print(floor)

    def part02(self):
        text = super()._part01_input()
        floor = 0
        index = 0
        for c in text:
            index += 1
            if c == '(':
                floor += 1
            else:
                floor -= 1
            if floor < 0:
                break
        print(index)
