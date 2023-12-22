import unittest
from adventofcode.year2023 import Day22


class TestDay22(unittest.TestCase):

    def setUp(self):
        self.day = Day22()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 428)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 35654)
