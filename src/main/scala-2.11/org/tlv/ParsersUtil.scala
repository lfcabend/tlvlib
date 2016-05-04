package org.tlv

import scala.util.parsing.combinator.Parsers

/**
  * Created by Lau on 4/24/2016.
  */
trait ParsersUtil extends Parsers {
  lazy val anyElem: Parser[Elem] = elem("anyElem", _ => true)

  def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => !(xs contains x))

  def elemOf(xs: Elem*): Parser[Elem] = elem("elemOf", xs contains _)

  def take(n: Int): Parser[Seq[Elem]] = repN(n, anyElem)

  def takeUntil(cond: Parser[Elem]): Parser[Seq[Elem]] = takeUntil(cond, anyElem)

  def takeUntil(cond: Parser[Elem], p: Parser[Elem]): Parser[Seq[Elem]] = rep(not(cond) ~> p)

  def takeWhile(p: Parser[Elem]): Parser[Seq[Elem]] = rep(p)
}
