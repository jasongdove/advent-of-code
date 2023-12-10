import unittest
from adventofcode.year2023 import Day05


class TestDay05(unittest.TestCase):

    def setUp(self):
        self.day = Day05()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 825516882)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 136096660)
