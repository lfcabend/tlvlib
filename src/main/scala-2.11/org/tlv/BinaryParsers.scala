package org.tlv

import scala.util.parsing.combinator.Parsers
import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import scala.language.implicitConversions

/**
  * Created by Lau on 4/24/2016.
  */
trait BinaryParsers extends Parsers with ParsersUtil {
  type Elem = Byte

  protected implicit def readerToByteReader(x: Input): ByteReader = x match {
    case br: ByteReader => br
    case _ => new ByteReader(x)
  }

  def toInt(bytes: Seq[Byte]): Int = bytes.foldLeft(0)((x, b) => (x << 8) + (b & 0xFF))

  def toLong(bytes: Seq[Byte]): Long = bytes.foldLeft(0L)((x, b) => (x << 8) + (b & 0xFF))

  lazy val byte: Parser[Byte] = anyElem
  lazy val u1: Parser[Int] = byte ^^ (_ & 0xFF)
  lazy val u2: Parser[Int] = bytes(2) ^^ toInt
  lazy val u4: Parser[Int] = bytes(4) ^^ toInt
  lazy val u4f: Parser[Float] = u4 ^^ intBitsToFloat
  lazy val u8: Parser[Long] = bytes(8) ^^ toLong
  lazy val u8d: Parser[Double] = u8 ^^ longBitsToDouble

  def bytes(n: Int): Parser[Seq[Byte]] = Parser { in =>
    if (n <= in.length) Success(in take n, in drop n)
    else Failure("Requested %d bytes but only %d remain".format(n, in.length), in)
  }

  def parse[T](p: Parser[T], in: Input): ParseResult[T] = p(in)

  def parse[T](p: Parser[T], in: String): ParseResult[T] = parse(p, new ByteReader(in))

  def parse[T](p: Parser[T], in: Seq[Byte]): ParseResult[T] = parse(p, new ByteReader(in))
}
