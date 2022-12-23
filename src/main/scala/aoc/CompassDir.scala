package aoc

enum CompassDir(val pos: Pos):
  case N extends CompassDir(Pos(-1, 0))
  case NE extends CompassDir(Pos(-1, 1))
  case E extends CompassDir(Pos(0, 1))
  case SE extends CompassDir(Pos(1, 1))
  case S extends CompassDir(Pos(1, 0))
  case SW extends CompassDir(Pos(1, -1))
  case W extends CompassDir(Pos(0, -1))
  case NW extends CompassDir(Pos(-1, -1))
