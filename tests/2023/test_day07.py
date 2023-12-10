import unittest
from adventofcode.year2023 import Day07


class TestDay07(unittest.TestCase):

    def setUp(self):
        self.day = Day07()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 250474325)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 248909434)
