import importlib
import sys

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print(f'usage: main.py [year] [day]')
    else:
        year_number = int(sys.argv[1])
        day_number = int(sys.argv[2])
        module = importlib.import_module(f'adventofcode.year{year_number}')
        day_class = getattr(module, f'Day{day_number:02}')
        day = day_class()
        p1 = day.part01()
        p2 = day.part02()
        print(f'{year_number} day {day_number}')
        print()
        print(f'P1: {p1}')
        print(f'P2: {p2}')
