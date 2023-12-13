from typing import Optional
import numpy as np

from adventofcode import Day


class Day13(Day):
    def __init__(self):
        super().__init__(2023, 13)

    @staticmethod
    def detect_horizontal_mirror(pattern: np.array) -> Optional[int]:
        #for row in pattern:
        #    print("".join(row))
        max_mirror = 0
        for i in range(0, len(pattern) - 1):
            low = i
            high = i + 1
            if "".join(pattern[low]) == "".join(pattern[high]):
                to_check = min(low, len(pattern) - high - 1)
                #print(f'low: {low}, high: {high}, to_check: {to_check} min({low}, {len(pattern) - high - 1})')
                is_mirror = True
                for j in range(1, to_check + 1):
                    if "".join(pattern[low - j]) != "".join(pattern[high + j]):
                        #print('not a mirror')
                        is_mirror = False
                        break
                if is_mirror:
                    max_mirror = max(max_mirror, high)
        #print()
        return None if max_mirror == 0 else max_mirror

    def part01(self):
        text = super()._part01_input()
        pattern = []
        total = 0
        for line in text.splitlines():
            if line == '':
                horizontal_mirror = self.detect_horizontal_mirror(np.array(pattern))
                if horizontal_mirror is not None:
                    #print(f'  - h: {horizontal_mirror}')
                    total += horizontal_mirror * 100
                #print()
                #print()
                #print()
                #print(np.array(pattern).transpose())
                vertical_mirror = self.detect_horizontal_mirror(np.array(pattern).transpose())
                if vertical_mirror is not None:
                    #print(f'  - v: {vertical_mirror}')
                    total += vertical_mirror
                pattern = []
            else:
                pattern.append([c for c in line])
        return total

    def part02(self):
        text = super()._part01_input()
        return 0
