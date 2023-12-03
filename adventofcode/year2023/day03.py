from adventofcode import Day
import re


class Day03(Day):
    def __init__(self):
        super().__init__(2023, 3)
        self.gn = re.compile('Game (\\d+): (.*)')
        self.clr = re.compile('(\\d+) ([a-z]+)')

    def part01(self):
        text = super()._part01_input()
        return 0

    def part02(self):
        text = super()._part01_input()
        return 0
