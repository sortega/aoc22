package aoc

enum CardinalPoint(val pos: Pos):
  case North extends CardinalPoint(Pos(row = -1, col = 0))
  case East extends CardinalPoint(Pos(row = 0, col = 1))
  case South extends CardinalPoint(Pos(row = 1, col = 0))
  case West extends CardinalPoint(Pos(row = 0, col = -1))
