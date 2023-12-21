import unittest
from adventofcode.year2023 import Day21


class TestDay21(unittest.TestCase):

    def setUp(self):
        self.day = Day21()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 0)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 0)
