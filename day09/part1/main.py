import copy
import math
from dataclasses import dataclass


@dataclass
class Position:
    x: int
    y: int

    def __hash__(self):
        return hash((self.x, self.y))


def distance(position1: Position, position2: Position) -> int:
    return int(math.sqrt((position2.x - position1.x) ** 2 + (position2.y - position1.y) ** 2))


def my_ceil(x: float) -> int:
    sign = 1
    if x < 0:
        sign = -1
    a = abs(x)
    ceil = math.ceil(a)
    return sign * ceil


def main():
    head_position = Position(0, 0)
    tail_position = Position(0, 0)
    tail_position_copy = copy.deepcopy(tail_position)
    tail_position_set = {tail_position_copy}
    with open("./input.txt") as input_file:
        for line in input_file:
            line = line.strip()
            direction, number_of_steps = line.split()
            for _ in range(int(number_of_steps)):
                if direction == "L":
                    head_position.x -= 1
                elif direction == "R":
                    head_position.x += 1
                elif direction == "D":
                    head_position.y -= 1
                elif direction == "U":
                    head_position.y += 1

                if distance(head_position, tail_position) > 1:
                    vector = (head_position.x - tail_position.x, head_position.y - tail_position.y)
                    unit_vector = (vector[0] / distance(head_position, tail_position),
                                   vector[1] / distance(head_position, tail_position))
                    movement_vector = (my_ceil(unit_vector[0]), my_ceil(unit_vector[1]))
                    tail_position.x += movement_vector[0]
                    tail_position.y += movement_vector[1]
                    tail_position_copy = copy.deepcopy(tail_position)
                    tail_position_set.add(tail_position_copy)
    print(len(tail_position_set))


if __name__ == '__main__':
    main()
