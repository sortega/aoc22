package aoc

import scala.annotation.tailrec

object Collections:
  def splitList[A](list: List[A])(isSeparator: A => Boolean): List[List[A]] =
    @tailrec
    def loop(list: List[A], groups: List[List[A]] = Nil): List[List[A]] =
      if (list.isEmpty) groups.reverse
      else
        val (group, rest) = list.dropWhile(isSeparator).span(a => !isSeparator(a))
        if (group.nonEmpty) loop(rest, group :: groups)
        else loop(rest, groups)

    loop(list)
    
  def mapForKeys[K, V](keys: Iterable[K])(f: K => V): Map[K, V] = keys.map(k => k -> f(k)).toMap
