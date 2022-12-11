package aoc

import cats.*
import cats.implicits.*
import scala.annotation.tailrec

object Functions:
  @tailrec
  def iterate[A](seed: A, times: Int)(f: A => A): A =
    if (times <= 0) seed
    else iterate(f(seed), times - 1)(f)

  def iterateM[A, F[_]: Monad](a: A, n: Int)(f: A => F[A]): F[A] =
    (0 -> a).tailRecM[F, A] {
      case (i, a) if i >= n => Right(a).pure
      case (i, a) => f(a).map(nextA => Left((i + 1) -> nextA))
    }
