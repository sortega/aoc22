package aoc

import cats.*
import cats.data.Ior
import cats.implicits.*

case class Matrix[A](rows: Vector[Vector[A]]):
  def apply(pos: Pos): A = apply(pos.row, pos.col)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def numRows: Int = rows.size
  def numCols: Int = rows.headOption.fold(0)(_.size)

  def values: Iterator[A] = rows.iterator.flatten

  def map[B](f: A => B): Matrix[B] = Matrix(rows.map(_.map(f)))
  def map[B](f: (Pos, A) => B): Matrix[B] = Matrix(
    rows.zipWithIndex.map {
      case (row, rowNumber) =>
        row.zipWithIndex.map {
          case (value, colNumber) =>
            f(Pos(rowNumber, colNumber), value)
        }
    }
  )
  def mapRows[B](f: Vector[A] => Vector[B]): Matrix[B] = Matrix(rows.map(f))
  def mapColumns[B](f: Vector[A] => Vector[B]): Matrix[B] = 
    Matrix(rows.transpose.map(f).transpose)

  def zipWith[B, C](other: Matrix[B])(f: (A, B) => C): Matrix[C] =
    Matrix.fromMap(toMap.align(other.toMap).collect {
      case (pos, Ior.Both(a, b)) => pos -> f(a, b)
    })
    
  def dotProduct(other: Matrix[A])(implicit num: Numeric[A]): Matrix[A] =
    zipWith(other) { (a, b) => num.times(a, b) }

  def reverseRows: Matrix[A] = Matrix(rows.map(_.reverse))

  def transpose: Matrix[A] = Matrix(rows.transpose)

  def toMap: Map[Pos, A] = (for {
    (row, rowNumber) <- rows.zipWithIndex
    (value, colNumber) <- row.zipWithIndex
  } yield Pos(rowNumber, colNumber) -> value).toMap

  override def toString: String = rows.map(_.mkString(" ")).mkString("\n")

object Matrix:
  def empty[A]: Matrix[A] = Matrix(Vector())

  def fromMap[A](map: Map[Pos, A]): Matrix[A] =
    fromMap(map, pos => throw new NoSuchElementException(s"no value for $pos"))

  def fromMap[A](map: Map[Pos, A], emptyValue: Pos => A): Matrix[A] = (for {
    maxRow <- map.keys.map(_.row).maxOption
    maxCol <- map.keys.map(_.col).maxOption
  } yield Matrix(
    Vector.tabulate(maxRow + 1, maxCol + 1) { (row, col) =>
      val pos = Pos(row, col)
      map.getOrElse(pos, emptyValue(pos))
    }
  )).getOrElse(empty)

  implicit val matrixApplicative: Apply[Matrix] = new Apply[Matrix]:
    override def map[A, B](fa: Matrix[A])(f: A => B): Matrix[B] = fa.map(f)
    override def ap[A, B](ff: Matrix[A => B])(fa: Matrix[A]): Matrix[B] =
      ff.zipWith(fa)((f, a) => f(a))
