from adventofcode import Day
import re

class Game:
    def __init__(self, number, red, green, blue):
        self.number = number
        self.red = red
        self.green = green
        self.blue = blue

    def __str__(self):
        return f'{self.number}: {self.red} R, {self.green} G, {self.blue} B'

    def is_possible(self, red, green, blue):
        return self.red <= red and self.green <= green and self.blue <= blue

class Day02(Day):
    def __init__(self):
        super().__init__(2023, 2)
        self.gn = re.compile('Game (\\d+): (.*)')
        self.clr = re.compile('(\\d+) ([a-z]+)')

    def games_from_line(self, line):
        match = self.gn.match(line)
        number = int(match.group(1))
        rest = match.group(2)
        games = []
        for hand in rest.split(';'):
            red = 0
            green = 0
            blue = 0
            for color in map(lambda x: x.strip(), hand.split(',')):
                color_match = self.clr.match(color)
                color_count = int(color_match.group(1))
                color_name =color_match.group(2)
                match color_name:
                    case 'red':
                        red += color_count
                    case 'green':
                        green += color_count
                    case 'blue':
                        blue += color_count
            games.append(Game(number, red, green, blue))
        return games

    def part01(self):
        text = super()._part01_input()
        max_red = 12
        max_green = 13
        max_blue = 14
        all_games = set()
        bad_games = set()
        for line in text.splitlines():
            games = self.games_from_line(line)
            for game in games:
                all_games.add(game.number)
                if not game.is_possible(max_red, max_green, max_blue):
                    bad_games.add(game.number)
        return sum(all_games.difference(bad_games))

    def part02(self):
        text = super()._part02_input()
        return 0
