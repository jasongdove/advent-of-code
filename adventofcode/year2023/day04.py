from adventofcode import Day
import re


class Card:

    def __init__(self, line):
        res = re.match('Card\s+(\d+):(.+)\|(.+)', line)
        self.card_number = res.group(1)
        self.winning_numbers = set(map(int, res.group(2).split()))
        self.all_numbers = set(map(int, res.group(3).split()))

    def score(self):
        count = len(self.all_numbers.intersection(self.winning_numbers))
        if count == 0:
            return 0
        return 2 ** (count - 1)


class Day04(Day):
    def __init__(self):
        super().__init__(2023, 4)

    def part01(self):
        text = super()._part01_input()
        cards = list(map(Card, text.splitlines()))
        return sum(map(lambda c: c.score(), cards))

    def part02(self):
        text = super()._part01_input()
        return 0
