from adventofcode import Day
from more_itertools import distinct_permutations as idp
import itertools
import re


class Day12(Day):
    def __init__(self):
        super().__init__(2023, 12)
        self.reg = re.compile('(#+)')
        self.bin_map = {'0': '.', '1': '#'}
        self.sets = {}

    def test_line(self, line: str, cont: list[int]) -> bool:
        sizes = self.reg.finditer(line)
        groups = [len(y) for y in [x.groups()[0] for x in sizes]]
        return groups == cont

    @staticmethod
    def to_binary(line: list[chr]) -> int:
        result = 0
        for index, c in enumerate(reversed(line)):
            if c == '#':
                result = result | (1 << index)
        return result

    @staticmethod
    def unknown_binary(line: list[chr]) -> int:
        result = 0
        for index, c in enumerate(reversed(line)):
            if c == '?':
                result = result | (1 << index)
        return result

    @staticmethod
    def reverse_mask(n: int, no_of_bits: int) -> int:
        result = 0
        for i in range(no_of_bits):
            result <<= 1
            result |= n & 1
            n >>= 1
        return result

    def bit_set_lists(self, n: int) -> list[int]:
        #if n not in self.sets:
            set_lens = []
            set_bits = 0
            span = 0
            while n:
                if n & 1:
                    set_bits += 1
                    span += 1
                else:
                    if span > 0:
                        set_lens.append(span)
                    span = 0
                n >>= 1
            if span > 0:
                set_lens.append(span)
            set_lens.reverse()
            return set_lens
            #self.sets[n] = set_lens
        #return self.sets[n]

    @staticmethod
    def count_set_bits(n):
        count = 0
        while n:
            count += n & 1
            n >>= 1
        return count

    def solve_line(self, line: str, unfold: int) -> int:
        s = line.split()
        r = s[0]
        c = list(map(int, s[1].split(',')))
        records = []
        cont = []
        for i in range(0, unfold):
            records.extend(r)
            records.append('?')
            cont.extend(c)
        records = records[:len(records)-1]

        b = Day12.to_binary(records)
        b2 = Day12.unknown_binary(records)
        b2_rev = Day12.reverse_mask(b2, len(records))
        print(f'known binary: {bin(b)[2:].zfill(len(records))}')
        print(f'unknown binary: {bin(b2)[2:].zfill(len(records))}')
        print(f'reverse unknown binary: {bin(b2_rev)[2:].zfill(len(records))}')
        print(r)
        print(cont)
        q_count = records.count('?')
        #print(q_count)
        maybe_lines = []
        total = 0
        total_set_bits = sum(cont)
        known_set_bits = Day12.count_set_bits(b)
        unknown_bits = Day12.count_set_bits(b2)
        j = total_set_bits - known_set_bits
        #print(f'need to test {j} bit(s)')
        bits = [1] * j
        for _ in range(0, unknown_bits - j):
            bits.append(0)
        #print(f'bits: {bits}')
        for k in idp(bits):
            #print(i)
            #if i % 10000 == 0:
                #print(i)
            #print(f'i: {i}, i_rev: {bin(i_rev)[2:].zfill(q_count)}')
            #print(f'final: {bin(final)[2:].zfill(len(records))}')
            #print(bin(i)[2:].zfill(q_count))
            t = b2_rev
            index = 0
            mask = 0
            for _ in range(0, len(records)):
                mask <<= 1
                #print(f'testing t & 1: {t & 1}')
                if t & 1:
                    mask += k[index] == 1
                    index += 1
                t >>= 1
            #print(f'mask: {bin(mask)[2:].zfill(len(records))}')

            final = mask | b

            #print(f'final: {bin(final)[2:].zfill(len(records))}')

            #set_lens = self.bit_set_lists(final)
            #print(set_lens)
            #if set_lens == cont:
            #    total += 1
                #print(f'need to test: {i}')
            #for j in range(0, len(records)):
            #    if records[j] == '?':
            #        maybe_line.append(binary.popleft())
            #    else:
            #        maybe_line.append(records[j])
            #maybe_lines.append("".join(maybe_line))
        print(f'total: {total}')
        return total

    def part01(self):
        text = super()._part01_input()
        #arrangements = [self.solve_line(x) for x in text.splitlines()]
        #return sum(arrangements)
        return 0

    def part02(self):
        text = super()._part01_input()
        arrangements = [self.solve_line(x, 5) for x in text.splitlines()[1:2]]
        return sum(arrangements)
