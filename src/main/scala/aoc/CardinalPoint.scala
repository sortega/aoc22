package aoc

enum CardinalPoint(val pos: Pos, val char: Char):
  case North extends CardinalPoint(Pos(row = -1, col = 0), '^')
  case East extends CardinalPoint(Pos(row = 0, col = 1), '>')
  case South extends CardinalPoint(Pos(row = 1, col = 0), 'v')
  case West extends CardinalPoint(Pos(row = 0, col = -1), '<')

  def turnClockwise: CardinalPoint = this match
    case North => East
    case East => South
    case South => West
    case West => North

  def turnCounterClockwise: CardinalPoint = this match
    case North => West
    case West => South
    case South => East
    case East => North
