from adventofcode import Day

class Map:

    def __init__(self, dest_start, source_start, length):
        self.dest_start = dest_start
        self.source_start = source_start
        self.length = length

    def map(self, value):
        if value in range(self.source_start, self.source_start + self.length):
            return self.dest_start + (value - self.source_start)
        return None


class MapSet:

    def __init__(self):
        self.maps = []

    def add(self, m):
        self.maps.append(m)

    def map(self, value):
        for m in self.maps:
            result = m.map(value)
            if result is not None:
                return result
        return value



class Day05(Day):
    def __init__(self):
        super().__init__(2023, 5)

    def part01(self):
        text = super()._part01_input()
        seeds = []
        seed_to_soil = MapSet()
        soil_to_fertilizer = MapSet()
        fertilizer_to_water = MapSet()
        water_to_light = MapSet()
        light_to_temp = MapSet()
        temp_to_humidity = MapSet()
        humidity_to_location = MapSet()
        lines = text.splitlines()
        for i in range(0, len(lines)):
            line = lines[i]
            if line.startswith("seeds:"):
                seeds.extend(map(int, filter(lambda x: x.isdigit(), line.split(':')[1].split(' '))))
            elif line.startswith("seed-to-soil map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    seed_to_soil.add(Map(values[0], values[1], values[2]))
            elif line.startswith("soil-to-fertilizer map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    soil_to_fertilizer.add(Map(values[0], values[1], values[2]))
            elif line.startswith("fertilizer-to-water map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    fertilizer_to_water.add(Map(values[0], values[1], values[2]))
            elif line.startswith("water-to-light map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    water_to_light.add(Map(values[0], values[1], values[2]))
            elif line.startswith("light-to-temperature map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    light_to_temp.add(Map(values[0], values[1], values[2]))
            elif line.startswith("temperature-to-humidity map"):
                while True:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    temp_to_humidity.add(Map(values[0], values[1], values[2]))
            elif line.startswith("humidity-to-location map"):
                while True and i < len(lines) - 1:
                    i += 1
                    line = lines[i]
                    if line == "":
                        break
                    values = list(map(int, line.split(' ')))
                    humidity_to_location.add(Map(values[0], values[1], values[2]))
        print(seeds)
        min_loc = 10000000000000
        for seed in seeds:
            soil = seed_to_soil.map(seed)
            fert = soil_to_fertilizer.map(soil)
            water = fertilizer_to_water.map(fert)
            light = water_to_light.map(water)
            temp = light_to_temp.map(light)
            humidity = temp_to_humidity.map(temp)
            loc = humidity_to_location.map(humidity)
            min_loc = min(min_loc, loc)
            print(f'{seed} => {soil} => {fert} => {water} => {light} => {temp} => {humidity} => {loc}')
        return min_loc

    def part02(self):
        text = super()._part01_input()
        return 0
