from __future__ import annotations
from collections import deque

import re
import math

from adventofcode import Day


class Pulse:
    def __init__(self, source: str, dest: str, high: bool):
        self.source = source
        self.dest = dest
        self.high = high

    def __repr__(self):
        return f'{self.source} => {self.dest} ({self.high})'


class Module:
    def __init__(self, name: str, destinations: list[str]):
        self.name = name
        self.destinations = destinations

    def pulse(self, high: bool, source: str) -> list[Pulse]:
        return []

    def __repr__(self):
        return f'{self.name} => {self.destinations}'


class Output(Module):
    def __init__(self, name: str):
        super().__init__(name, [])


class Broadcast(Module):
    def __init__(self, name: str, destinations: list[str]):
        super().__init__(name, destinations)

    def pulse(self, high: bool, source: str) -> list[Pulse]:
        result = []
        for module in self.destinations:
            result.append(Pulse(self.name, module, high))
        return result


class FlipFlop(Module):
    def __init__(self, name: str, destinations: list[str]):
        super().__init__(name, destinations)
        self.state = False

    def pulse(self, high: bool, source: str) -> list[Pulse]:
        result = []
        if not high:
            self.state = not self.state
            for module in self.destinations:
                result.append(Pulse(self.name, module, self.state))
        return result


class Conjunction(Module):
    def __init__(self, name: str, destinations: list[str]):
        super().__init__(name, destinations)
        self.mem: dict[str, bool] = {}

    def pulse(self, high: bool, source: str) -> list[Pulse]:
        result = []
        self.mem[source] = high
        if all(v == True for v in self.mem.values()):
            for module in self.destinations:
                result.append(Pulse(self.name, module, False))
        else:
            for module in self.destinations:
                result.append(Pulse(self.name, module, True))
        return result

    def add_input(self, name: str):
        self.mem[name] = False

    def __repr__(self):
        return f'{self.name} => {self.destinations} (mem={self.mem})'


class Day20(Day):
    def __init__(self):
        super().__init__(2023, 20)
        self.reg = re.compile(r'([%&\w]+)\s->\s(\w+.*)+')

    def build_modules(self, text: str) -> dict[str, Module]:
        modules: dict[str, Module] = {}

        for line in text.splitlines():
            reg = self.reg.match(line)
            module = reg.group(1)
            destinations: list[str] = reg.group(2).split(', ')
            # flip flop
            if module.startswith('%'):
                name = module[1:]
                module = FlipFlop(name, destinations)
                modules[name] = module
            # conjunction
            elif module.startswith('&'):
                name = module[1:]
                module = Conjunction(name, destinations)
                modules[name] = module
            # broadcast
            elif module == 'broadcaster':
                modules[module] = Broadcast(module, destinations)

        to_add = set()
        for m in modules.values():
            for destination in m.destinations:
                if destination not in modules:
                    to_add.add(destination)
                else:
                    dest_mod = modules[destination]
                    if isinstance(dest_mod, Conjunction):
                        dest_mod.add_input(m.name)

        for name in to_add:
            modules[name] = Output(name)

        return modules

    @staticmethod
    def find_cycle_length(target: str, modules: dict[str, Module]) -> int:
        target_module = modules[target]
        inputs = set()
        i = 0
        while True:
            q = deque([Pulse('btn', 'broadcaster', False)])
            i += 1
            while len(q) > 0:
                pulse = q.popleft()
                mod = modules[pulse.dest]
                next_pulse = mod.pulse(pulse.high, pulse.source)
                q.extend(next_pulse)
                if mod == target_module and pulse.high:
                    inputs.add(i)
                    if len(inputs) == 4:
                        return math.lcm(*inputs)

    def part01(self):
        text = super()._part01_input()
        modules = self.build_modules(text)

        low_count = 0
        high_count = 0

        for _ in range(1000):
            q = deque([Pulse('btn', 'broadcaster', False)])
            while len(q) > 0:
                pulse = q.popleft()
                low_count += 1 if not pulse.high else 0
                high_count += 1 if pulse.high else 0
                q.extend(modules[pulse.dest].pulse(pulse.high, pulse.source))

        return low_count * high_count

    def part02(self):
        text = super()._part01_input()
        modules = self.build_modules(text)

        rx_parent = 'rx'
        for module in modules.values():
            if 'rx' in module.destinations:
                if isinstance(module, Conjunction):
                    rx_parent = module.name

        return Day20.find_cycle_length(rx_parent, modules)
