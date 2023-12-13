from typing import Optional
import numpy as np

from adventofcode import Day


class Day13(Day):
    def __init__(self):
        super().__init__(2023, 13)

    @staticmethod
    def detect_horizontal_mirror(pattern: np.array, ignore: Optional[int]) -> Optional[int]:
        max_mirror = 0
        for i in range(0, len(pattern) - 1):
            low = i
            high = i + 1
            if ignore == high:
                continue
            if "".join(pattern[low]) == "".join(pattern[high]):
                to_check = min(low, len(pattern) - high - 1)
                is_mirror = True
                for j in range(1, to_check + 1):
                    if "".join(pattern[low - j]) != "".join(pattern[high + j]):
                        is_mirror = False
                        break
                if is_mirror:
                    max_mirror = max(max_mirror, high)
        return None if max_mirror == 0 else max_mirror

    @staticmethod
    def detect_smudge(pattern: list[list[chr]]) -> int:
        change = {'.': '#', '#': '.'}

        original_horizonal = Day13.detect_horizontal_mirror(np.array(pattern), None)
        original_vertical = Day13.detect_horizontal_mirror(np.array(pattern).transpose(), None)

        for row_index in range(len(pattern)):
            for col_index in range(len(pattern[0])):
                p = np.array(pattern)
                p[row_index][col_index] = change[p[row_index][col_index]]

                horizontal_mirror = Day13.detect_horizontal_mirror(p, original_horizonal)
                if horizontal_mirror is not None and horizontal_mirror != original_horizonal:
                    return horizontal_mirror * 100

                vertical_mirror = Day13.detect_horizontal_mirror(p.transpose(), original_vertical)
                if vertical_mirror is not None and vertical_mirror != original_vertical:
                    return vertical_mirror
        return 0

    def part01(self):
        text = super()._part01_input()
        pattern = []
        total = 0
        for line in text.splitlines():
            if line == '':
                horizontal_mirror = self.detect_horizontal_mirror(np.array(pattern), None)
                if horizontal_mirror is not None:
                    total += horizontal_mirror * 100

                vertical_mirror = self.detect_horizontal_mirror(np.array(pattern).transpose(), None)
                if vertical_mirror is not None:
                    total += vertical_mirror

                pattern = []
            else:
                pattern.append([c for c in line])
        return total

    def part02(self):
        text = super()._part01_input()
        pattern = []
        total = 0
        for line in text.splitlines():
            if line == '':
                total += self.detect_smudge(pattern)
                pattern = []
            else:
                pattern.append([c for c in line])
        return total
