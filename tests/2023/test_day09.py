import unittest
from adventofcode.year2023 import Day09


class TestDay01(unittest.TestCase):

    def setUp(self):
        self.day = Day09()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 1681758908)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 803)
