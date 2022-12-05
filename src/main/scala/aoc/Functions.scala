package aoc

import scala.annotation.tailrec

object Functions:
  @tailrec
  def iterate[A](seed: A, times: Int)(f: A => A): A =
    if (times <= 0) seed
    else iterate(f(seed), times - 1)(f)
