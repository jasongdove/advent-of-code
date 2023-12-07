from adventofcode import Day
from collections import Counter


class Hand:
    def __init__(self, line):
        s = line.split(' ')
        self.cards = [c for c in s[0]]
        self.bid = int(s[1])

    def __str__(self):
        return f'{self.cards} => {self.bid} => {self.get_type()} => {self.get_card_ranks()}'

    def get_type(self):
        res = list(reversed(sorted(Counter(self.cards).values())))
        if res[0] == 5:
            return 7
        elif res[0] == 4:
            return 6
        elif res[0] == 3 and res[1] == 2:
            return 5
        elif res[0] == 3:
            return 4
        elif res[0] == 2 and res[1] == 2:
            return 3
        elif res[0] == 2:
            return 2
        else:
            return 1

    def get_card_ranks(self):
        res = []
        for card in self.cards:
            if card == 'A':
                res.append('A')
            elif card == 'K':
                res.append('B')
            elif card == 'Q':
                res.append('C')
            elif card == 'J':
                res.append('D')
            elif card == 'T':
                res.append('E')
            elif card == '9':
                res.append('F')
            elif card == '8':
                res.append('G')
            elif card == '7':
                res.append('H')
            elif card == '6':
                res.append('I')
            elif card == '5':
                res.append('J')
            elif card == '4':
                res.append('K')
            elif card == '3':
                res.append('L')
            elif card == '2':
                res.append('M')
        return "".join(res)

    def __eq__(self, obj):
        if self.get_type() != obj.get_type():
            return False
        return self.get_card_ranks() == obj.get_card_ranks()

    def __lt__(self, obj):
        if self.get_type() < obj.get_type():
            return True
        if self.get_type() > obj.get_type():
            return False
        return self.get_card_ranks() > obj.get_card_ranks()

    def __le__(self, obj):
        if self.get_type() > obj.get_type():
            return True
        if self.get_type() < obj.get_type():
            return False
        return self.get_card_ranks() < obj.get_card_ranks()


class Hand2(Hand):
    def __init__(self, line):
        super().__init__(line)

    def get_type(self):
        c = Counter(self.cards)
        j = c['J']
        del c['J']
        res = list(reversed(sorted(c.values())))
        if len(res) == 0 or res[0] == 5 or (res[0] + j == 5):
            return 7
        elif res[0] == 4 or (res[0] + j == 4):
            return 6
        elif res[0] == 3 and res[1] == 2 or (res[0] + j == 3 and res[1] == 2) or (res[0] == 3 and res[1] + j == 2):
            return 5
        elif res[0] == 3 or (res[0] + j == 3):
            return 4
        elif res[0] == 2 and res[1] == 2 or (res[0] == 2 and res[1] + j == 2):
            return 3
        elif res[0] == 2 or (res[0] + j == 2):
            return 2
        else:
            return 1

    def get_card_ranks(self):
        res = []
        for card in self.cards:
            if card == 'A':
                res.append('A')
            elif card == 'K':
                res.append('B')
            elif card == 'Q':
                res.append('C')
            elif card == 'T':
                res.append('E')
            elif card == '9':
                res.append('F')
            elif card == '8':
                res.append('G')
            elif card == '7':
                res.append('H')
            elif card == '6':
                res.append('I')
            elif card == '5':
                res.append('J')
            elif card == '4':
                res.append('K')
            elif card == '3':
                res.append('L')
            elif card == '2':
                res.append('M')
            elif card == 'J':
                res.append('N')
        return "".join(res)


class Day07(Day):
    def __init__(self):
        super().__init__(2023, 7)

    @staticmethod
    def solve(hand_type, text):
        hands = list(map(hand_type, text.splitlines()))
        res = sorted(hands)
        winnings = 0
        for i in range(len(res)):
            hand = res[i]
            winnings += (i + 1) * hand.bid
        return winnings

    def part01(self):
        text = super()._part01_input()
        return Day07.solve(Hand, text)

    def part02(self):
        text = super()._part02_input()
        return Day07.solve(Hand2, text)
