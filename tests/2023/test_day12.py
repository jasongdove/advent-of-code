import unittest
from adventofcode.year2023 import Day12


class TestDay12(unittest.TestCase):

    def setUp(self):
        self.day = Day12()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 7771)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 10861030975833)
