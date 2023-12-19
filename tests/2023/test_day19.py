import unittest
from adventofcode.year2023 import Day19


class TestDay19(unittest.TestCase):

    def setUp(self):
        self.day = Day19()

    def test_part01(self):
        p1 = self.day.part01()
        # 167409079868000
        self.assertEqual(p1, 395382)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 103557657654583)
