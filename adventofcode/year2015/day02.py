from adventofcode import Day
from functools import reduce


class Present:
    def __init__(self, length, width, height):
        self.length = length
        self.width = width
        self.height = height

    def __str__(self):
        return f'{self.length}x{self.width}x{self.height}: {self.shortest_sides()}'

    def all_sides(self):
        return [self.length, self.width, self.height]

    def shortest_sides(self):
        sides = self.all_sides()
        sides.remove(max(sides))
        return sides

    def required_paper(self):
        slack = reduce(lambda x, y: x * y, self.shortest_sides(), 1)
        return (2 * self.length * self.width +
                2 * self.width * self.height +
                2 * self.height * self.length) + slack

    def required_ribbon(self):
        ribbon = sum(map(lambda x: x + x, self.shortest_sides()))
        return ribbon + reduce(lambda x, y: x * y, self.all_sides(), 1)


class Day02(Day):
    def __init__(self):
        super().__init__(2015, 2)

    def part01(self):
        text = super()._part01_input()
        total = 0
        for line in text.splitlines():
            split = list(map(int, line.split('x')))
            present = Present(split[0], split[1], split[2])
            total += present.required_paper()
        return total

    def part02(self):
        text = super()._part01_input()
        total = 0
        for line in text.splitlines():
            split = list(map(int, line.split('x')))
            present = Present(split[0], split[1], split[2])
            total += present.required_ribbon()
        return total
