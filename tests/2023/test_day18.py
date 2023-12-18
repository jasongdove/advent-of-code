import unittest
from adventofcode.year2023 import Day18


class TestDay18(unittest.TestCase):

    def setUp(self):
        self.day = Day18()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 49061)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 92556825427032)
