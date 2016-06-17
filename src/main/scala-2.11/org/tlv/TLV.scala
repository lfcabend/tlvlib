package org.tlv

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by lau on 1-7-15.
  */

object TLV {

  import scala.language.implicitConversions

  implicit def byteSequenceWrapper(s: String): Seq[Byte] = HexUtils.hex2Bytes(s)

  trait Tag {

    val value: Seq[Byte]

    lazy val length: Int = value.length

    override def toString() = value.map("%02X" format _).mkString

  }

  trait TLV[T0 <: Tag, T1 <: TLV[_, _]] extends Traversable[T1] {

    val tag: T0

    val value: Seq[Byte]

    lazy val length: Int = value.length

    def serializeTLV: Seq[Byte]

    def pretty: String

  }

  case class BerTag(val value: Seq[Byte]) extends Tag {
    require(Option(value).map(!_.isEmpty) == Option(true), "value is null or empty")

    private val hasNextByte: Seq[Boolean] =
      value.zipWithIndex.map { case (x, index) => shouldHaveNextByte(x, index) }

    require(hasNextByte.reduceRight(_ && _), "Invalid length indicators")

    def isConstructed: Boolean =
      value.lift(0).map(x => (x & 0x20) == 0x20).getOrElse(false)

    def isUniversalClass: Boolean =
      value.lift(0).map(x => (~x & 0xC0) == 0xC0).getOrElse(false)

    def isApplicationClass: Boolean =
      value.lift(0).map(x => (x & 0x40) == 0x40).getOrElse(false)

    def isContextSpecifClass: Boolean =
      value.lift(0).map(x => (x & 0x80) == 0x80).getOrElse(false)

    def isPrivateClass: Boolean =
      value.lift(0).map(x => (x & 0xC0) == 0xC0).getOrElse(false)

    private def shouldHaveNextByte(x: Byte, index: Int) = {
      val size = value.length
      val hasNext = BerTag.hasNextByte(x, index)
      if (index < size - 1) hasNext else !hasNext
    }

  }

  object BerTag {

    def hasNextByte(x: Byte, index: Int) = index match {
      case 0 => (x & 0x1F) == 0x1F
      case _ => (x & 0x80) == 0x80
    }

  }

  abstract class PathX {

    def tag: BerTag

  }

  sealed case class PathEx(tag: BerTag) extends PathX

  sealed case class PathExIndex(tag: BerTag, index: Int = 0) extends PathX


  trait BerTLV extends TLV[BerTag, BerTLV] {

    def select(expression: List[PathX]): Option[List[BerTLV]] = selectInternal(expression)._1

    def ->(expression: PathX*) = select(expression.toList)

    def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX])

    def prettyWithDepth(depth: Int): String

    def updated(tlv: BerTLV): BerTLV

    def updated(expression: List[PathX], tlv: BerTLV): BerTLV = updatedInternal(expression, tlv)._1

    def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX])

    def foldTLV[B](f: (BerTag, Seq[Byte]) => B, g: (BerTag, Seq[B]) => B): B

    override def serializeTLV: Seq[Byte] = {
      val f: (BerTag, Seq[Byte]) => Seq[Byte] =
        (t, v) => t.value ++ BerTLV.encodeLength(v) ++ v
      val g: (BerTag, Seq[Seq[Byte]]) => Seq[Byte] = (t, v) => {
        val cat: Seq[Byte] = v.flatten
        t.value ++ BerTLV.encodeLength(cat) ++ cat
      }
      foldTLV(f, g)
    }

  }

  object BerTLV {

    def encodeLengthOld(v: Seq[Byte]): Seq[Byte] = {
      val length = v.length
      if (length <= 0x7F)
        Array[Byte](length.toByte)
      else if (length <= 0xFF)
        Array[Byte](0x81.toByte, length.toByte)
      else if (length <= 0xFFFF)
        Array[Byte](0x82.toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
      else if (length <= 0xFFFFFF)
        Array[Byte](0x83.toByte, ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
      else //if (length <= 0x7FFFFFFF) {
        Array[Byte](0x84.toByte, ((length >> 24) & 0xFF).toByte,
          ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
    }

    def encodeLength(v: Seq[Byte]): Seq[Byte] = {
      val length = v.length
      encodeLength(length)
    }

    def encodeLength(length: Int): Seq[Byte] = {
      lazy val theRest: Int => List[Byte] = x =>
        if (x == 0) List(length.toByte)
        else ((length >> (x * 8)) & 0xFF).toByte :: theRest(x - 1)
      val lengthEncoded: Int => List[Byte] = x => if (x > 0) ((0x80 | x).toByte) :: theRest(x - 1) else theRest(x)

      //TODO change with log that might be expensive
      if (length <= 0x7F)
        lengthEncoded(0)
      else if (length <= 0xFF)
        lengthEncoded(1)
      else if (length <= 0xFFFF)
        lengthEncoded(2)
      else if (length <= 0xFFFFFF)
        lengthEncoded(3)
      else //if (length <= 0x7FFFFFFF) {
        lengthEncoded(4)
    }
  }

  implicit def strToPathEx(s: String) = new PathEx(new BerTag(s))

  implicit def strAndIndexToPathEx(x: (String, Int)) = new PathExIndex(new BerTag(x._1), x._2)

  implicit def seqToTLVSeq(s: List[BerTLV]) = new TLVSeq(s)

  class TLVSeq(s: List[BerTLV]) {

    def >>:(tag: BerTag) = BerTLVCons(tag, s)

  }

  implicit def stringToTLVValue(s: String) = new TLVValue(s)

  class TLVValue(s: String) {

    def >>:(tag: BerTag) = BerTLVLeaf(tag, HexUtils.hex2Bytes(s))

  }

  implicit def stringToBerTag(s: String) = BerTag(HexUtils.hex2Bytes(s))


  implicit def tlvToTLVCons(tlv: BerTLV) = new TLVCons(tlv)

  class TLVCons(tlv: BerTLV) {

    def >>:(tag: BerTag) = BerTLVCons(tag, List(tlv))

  }

  case class BerTLVLeaf(val tag: BerTag, val value: Seq[Byte]) extends BerTLVLeafT

  trait BerTLVLeafT extends BerTLV {

    //    require(tag != null, "tag is null")
    //    require(value != null, "value is null")


    override def updated(tlv: BerTLV): BerTLV =
      if (tlv.tag == tag) tlv
      else this

    override def toString() = s"BerTLVLeaf($tag, $value)"

    override def foreach[U](f: BerTLV => U): Unit = f(this)

    def foldTLV[B](f: (BerTag, Seq[Byte]) => B, g: (BerTag, Seq[B]) => B): B = f(tag, value)


    override def pretty: String =
      tag.toString() + " " + value.map("%02X" format _).mkString + "\n"

    def prettyWithDepth(depth: Int): String =
      "\t" * depth + pretty

    override def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX]) = expression match {
      case ((x: PathEx) :: Nil) if x.tag == tag =>
        (Some(List(this)), expression)
      case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
        (Some(List(this)), List(PathExIndex(x.tag, x.index - 1)))
      case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
        (None, List(PathExIndex(x.tag, x.index - 1)))
      case _ =>
        (None, expression)
    }

    override def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX]) = expression match {
      case ((x: PathEx) :: Nil) if x.tag == tag =>
        (tlv, expression)
      case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
        (tlv, List(PathExIndex(x.tag, x.index - 1)))
      case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
        (this, List(PathExIndex(x.tag, x.index - 1)))
      case _ =>
        (this, expression)
    }

  }

  case class BerTLVCons(val tag: BerTag, val constructedValue: List[BerTLV]) extends BerTLVConsT

  trait BerTLVConsT extends BerTLV {
    //    require(tag != null, "tag is null")
    //    require(tag.isConstructed, "need a constructed tag")
    //    require(constructedValue != null, "value is null or empty")

    def constructedValue: List[BerTLV]

    override def toString() = s"BerTLVCons($tag, $constructedValue)"

    def foreach[U](f: BerTLV => U): Unit = {
      f(this)
      constructedValue.foreach({ x =>
        x.foreach(f)
      })
    }

    def foldTLV[B](f: (BerTag, Seq[Byte]) => B, g: (BerTag, Seq[B]) => B): B = g(tag, constructedValue.map(_.foldTLV(f, g)))

    def updated(tlv: BerTLV): BerTLV =
      if (tlv.tag == tag) tlv
      else BerTLVCons(tag, constructedValue.map(_.updated(tlv)))

    override val value: Seq[Byte] = constructedValue.flatMap(x => x.serializeTLV)

    override def pretty: String = prettyWithDepth(0)

    override def prettyWithDepth(depth: Int): String =
      "\t" * depth + tag.toString() + "\n" + constructedValue.map(_.prettyWithDepth(depth + 1)).mkString

    override def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX]) = {
      val foldFunc: (BerTLV, (Option[List[BerTLV]], List[PathX])) => (Option[List[BerTLV]], List[PathX]) = {
        (tlv, currentResult) => {
          val (cTLV, _) = currentResult
          val (nTLV, nEx) = tlv.selectInternal(currentResult._2)
          val r = (cTLV, nTLV) match {
            case (Some(v1), Some(v2)) => Some(v1 ++ v2)
            case (None, Some(v2)) => Some(v2)
            case (Some(v1), None) => Some(v1)
            case _ => None
          }
          (r, nEx)
        }
      }
      val cons: Option[List[BerTLV]] => Option[List[BerTLV]] = x1 => x1.map(x => List(BerTLVCons(tag, x)))
      val z0: (Option[List[BerTLV]], List[PathX]) = (None, expression.tail)
      val z1: (Option[List[BerTLV]], List[PathX]) = (None, expression)
      expression match {
        case ((x: PathEx) :: Nil) if x.tag == tag =>
          (Some(List(this)), x :: Nil)
        case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
          (Some(List(this)), List(PathExIndex(x.tag, x.index - 1)))
        case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
          (None, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur
        case ((x: PathEx) :: xs) if x.tag == tag =>
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (cons(r._1), x :: r._2)
        case ((x: PathExIndex) :: xs) if x.index == 0 && x.tag == tag =>
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (cons(r._1), PathExIndex(x.tag, x.index - 1) :: r._2)
        case ((x: PathExIndex) :: xs) if x.index != 0 && x.tag == tag =>
          (None, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
        case ex@(x :: xs) if x.tag != tag =>
          constructedValue.foldLeft(z1)((p1, p2) => foldFunc(p2, p1))
        case _ =>
          (None, expression)
      }
    }

    override def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX]) = {
      val foldFunc: (BerTLV, (List[BerTLV], List[PathX])) => (List[BerTLV], List[PathX]) = {
        (tlv, currentResult) => {
          val (cTLV, _) = currentResult
          val (nTLV, nEx) = tlv.updatedInternal(currentResult._2, tlv)
          (cTLV ++ nTLV, nEx)
        }
      }
      val z0: (List[BerTLV], List[PathX]) = (Nil, expression.tail)
      val z1: (List[BerTLV], List[PathX]) = (Nil, expression)
      expression match {
        case ((x: PathEx) :: Nil) if x.tag == tag =>
          (tlv, x :: Nil)
        case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
          (tlv, List(PathExIndex(x.tag, x.index - 1)))
        case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
          (this, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur
        case ((x: PathEx) :: xs) if x.tag == tag =>
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r._1), x :: r._2)
        case ((x: PathExIndex) :: xs) if x.index == 0 && x.tag == tag =>
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r._1), PathExIndex(x.tag, x.index - 1) :: r._2)
        case ((x: PathExIndex) :: xs) if x.index != 0 && x.tag == tag =>
          (this, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
        case ex@(x :: xs) if x.tag != tag =>
          val r = constructedValue.foldLeft(z1)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r._1), r._2)
        case _ =>
          (this, expression)
      }
    }

  }

  trait TLVParsers extends BinaryParsers {

    import java.math.BigInteger

    import scala.language.postfixOps

    def parseTLV(in: String) = parse(parseATLV, in)

    def parseTLVList(in: String) = parse(parseATLV +, in)

    def parseTLV(in: Seq[Byte]) = parse(parseATLV, in)

    def parseTLVList(in: Seq[Byte]) = parse(parseATLV +, in)

    def parseATLV: Parser[BerTLV] = parseAMultipleTLV | parseASingleTLV

    def parseAMultipleTLV: Parser[BerTLVCons] = parseAConstructedTag ~
      parseALength.flatMap(x => repParsingTLVForXByte(x)) ^^ { case t ~ c => BerTLVCons(t, c) }

    def parseASingleTLV: Parser[BerTLVLeaf] = parseNonConstructedATag ~ parseALength.
      into(x => repN(x, parseSingleByte)) ^^ {
      case t ~ v => BerTLVLeaf(t, v)
    }

    def parseLeafTag(tag: BerTag): Parser[BerTag] =
      parseNonConstructedATag.
        withFilter(_ == tag).
        withFailureMessage(s"Tag is not ${tag}")

    def parseConsTag(tag: BerTag): Parser[BerTag] =
      parseAConstructedTag.
        withFilter(_ == tag).
        withFailureMessage(s"Tag is not ${tag}")

    def parseVarLength(upto: Int): Parser[Int] =
      parseALength.withFilter(_ <= upto).withFailureMessage(s"Maximum length is ${upto}")

    def parseLength(length: Int): Parser[Int] =
      parseALength.withFilter(_ == length).withFailureMessage(s"length should be ${length}")

    def parseVarLengthBetween(min: Int, max: Int): Parser[Int] =
      parseALength.withFilter((x: Int) => x >= min && x <= max).
        withFailureMessage(s"length should be between min ${min} and max ${max}")

    def parseNonConstructedATag: Parser[BerTag] = parseATag.withFilter(!_.isConstructed).
      withFailureMessage("tag is not a non-constructed tag")

    def parseAConstructedTag: Parser[BerTag] = parseATag.withFilter(_.isConstructed).
      withFailureMessage("tag is not a constructed tag")

    def parseALength: Parser[Int] = (parseSingleLength ^^ (_.toInt)) |
      (parseMultipleLength ^^ (x => {
        BigInt(1, x.toArray).intValue()
      }))

    def parseSingleLength = parseSingleByte.
      filter((x: Byte) => {
        (x & 0xFF) <= 0x7F
      }).withFailureMessage("Byte is more then 0x7F")

    def parseMultipleLength = firstByteOfMultipleLength.into(parserNBytesOfLength)

    def parserNBytesOfLength: (Int => Parser[List[Byte]]) = (x => repN(x, parseSingleByte))

    def firstByteOfMultipleLength = parseSingleByte.
      filter((x: Byte) => {
        (x & 0xFF) > 0x7F
      }).map((x: Byte) => {
      (x & 0xFF) & 0x7F
    }).
      withFailureMessage("Byte is not more then 0x7F")


    def parseATag: Parser[BerTag] = parseMoreByteTag | parseTwoByteTag | parseSingleByteTag

    def parseMoreByteTag = parseFirstByteOfTagWithMore ~ (parseSubsequentByteOfTagWithMore *) ~ parseSingleByte ^^ {
      case f ~ s ~ l => BerTag(f :: s ++ List(l))
    }

    def parseTwoByteTag = parseFirstByteOfTagWithMore ~ parseSingleByte ^^ { case f ~ l => BerTag(f :: List(l)) }

    def parseSingleByteTag = parseSingleByte ^^ { case l => BerTag(List(l)) }

    def parseFirstByteOfTagWithMore = parseSingleByte.
      filter(BerTag.hasNextByte(_, 0)).withFailureMessage("Byte does not indicate more bytes")

    def parseSubsequentByteOfTagWithMore: Parser[Byte] = parseSingleByte.
      filter(BerTag.hasNextByte(_, 1)).withFailureMessage("Byte does not indicate more bytes")


    def parseSingleByte: Parser[Byte] = new Parser[Byte] {
      def apply(in: Input): ParseResult[Byte] = {
        if (!in.atEnd) Success(in.first, in.rest)
        else Failure("End of stream", in)
      }
    }

    def parseTlvForXBytes(totalSize: Int): Parser[BerTLV] =
      parseASingleTLVForXBytes(totalSize) | parseAMultipleTLVForXBytes(totalSize)

    def parseAMultipleTLVForXBytes(totalSize: Int): Parser[BerTLVCons] = parseAConstructedTag ~
      parseALength.into(x => repParsingTLVForXByte(totalSize - x)) ^^ { case t ~ c => BerTLVCons(t, c) }

    def parseASingleTLVForXBytes(totalSize: Int): Parser[BerTLVLeaf] = parseASingleTLV

    def repParsingTLVForXByte(totalSize: Int): Parser[List[BerTLV]] = repParsingForXByte(totalSize, parseATLV)

    def repParsingForXByte[A](totalSize: Int, parser: Parser[A]): Parser[List[A]] = Parser { in =>
      val elems = new ListBuffer[A]
      def continue(in: Input, size: Int): ParseResult[List[A]] = {
        @tailrec def applyP(in0: Input, size: Int): ParseResult[List[A]] = {
          parser(in0) match {
            case Success(x, rest) if (consumed(in0, rest) < size) => {
              elems += x
              applyP(rest, size - consumed(in, rest))
            }
            case e@Error(_, _) => e // still have to propagate error
            case f@Failure(_, _) => f
            case Success(x, rest) => elems += x; Success(elems.toList, rest)
          }
        }

        applyP(in, size)
      }

      if (totalSize <= 0) {
        Success(Nil, in)
      } else {
        continue(in, totalSize)
      }
    }

    private def consumed(in0: Input, in1: Input): Int = in1.offset - in0.offset

  }

}
