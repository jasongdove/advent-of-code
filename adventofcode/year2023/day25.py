from collections import deque

from adventofcode import Day


class Day25(Day):
    def __init__(self):
        super().__init__(2023, 25)

    @staticmethod
    def count_nodes(adj: dict[str, list[str]], node: str) -> int:
        q = deque([node])
        visited = set()
        while len(q) > 0:
            n = q.popleft()
            if n not in visited:
                visited.add(n)
                for a in adj[n]:
                    q.append(a)
        return len(visited)

    def part01(self):
        text = super()._part01_input()
        adj = {}
        # thanks, graphviz
        to_cut = [('znk', 'mmr'), ('vcq', 'lxb'), ('rnx', 'ddj')]
        for line in text.splitlines():
            parts = line.split(':')
            n = parts[0].strip()
            other = parts[1].strip().split(' ')
            for o in other:
                if (n, o) in to_cut or (o, n) in to_cut:
                    continue
                if n not in adj:
                    adj[n] = []
                adj[n].append(o)
                if o not in adj:
                    adj[o] = []
                adj[o].append(n)
        return self.count_nodes(adj, 'pvq') * self.count_nodes(adj, 'qtp')

    def part02(self):
        return 0
