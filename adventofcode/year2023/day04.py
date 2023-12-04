from adventofcode import Day
import re


class Card:

    def __init__(self, line):
        res = re.match("Card\\s+(\\d+):(.+)\\|(.+)", line)
        self.card_number = res.group(1)
        self.winning_numbers = set(map(int, res.group(2).split()))
        self.all_numbers = set(map(int, res.group(3).split()))

    def score(self):
        count = self.matching()
        if count == 0:
            return 0
        return 2 ** (count - 1)

    def matching(self):
        return len(self.all_numbers.intersection(self.winning_numbers))


class Day04(Day):
    def __init__(self):
        super().__init__(2023, 4)

    def part01(self):
        text = super()._part01_input()
        cards = map(Card, text.splitlines())
        return sum(map(lambda c: c.score(), cards))

    def part02(self):
        text = super()._part01_input()
        cards = list(map(Card, text.splitlines()))
        card_counts = [1] * len(cards)
        for i in range(0, len(cards)):
            count = cards[i].matching()
            for j in range(0, card_counts[i]):
                for k in range(i + 1, i + 1 + count):
                    card_counts[k] += 1
        return sum(card_counts)
